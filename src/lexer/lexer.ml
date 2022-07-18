let digit = [%sedlex.regexp? '0' .. '9']
let positive_digit = [%sedlex.regexp? '1' .. '9']
let white_space = [%sedlex.regexp? Chars " \t\n\r"]
let newline = [%sedlex.regexp? "\r\n" | "\r" | "\n"]
let hex = [%sedlex.regexp? 'a' .. 'f' | 'A' .. 'F' | digit]

(* a number contains 1 or more digits and does not start with 0*)
let number = [%sedlex.regexp? '0' | positive_digit, Star digit]

(* characters accepted in idenifiers used in evo files *)
let name_unit = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | digit | '_']
let identifier = [%sedlex.regexp? 'a' .. 'z', Star name_unit]
let module_name = [%sedlex.regexp? 'A' .. 'Z', Star name_unit]

open Parse.Parser

exception Exceeded_maximum_int of string
exception Invalid_escape of string
exception Unclosed_literal of string
exception Unsupported_code_point of string
exception Invalid_character

let buffer_queue = Queue.create ()
let reset_queue () = Queue.clear buffer_queue
let store_unicode (code : int) = Queue.add code buffer_queue

let print_pos (pos : Lexing.position) =
  print_endline
    (Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1))

let sequence_lexed : bool ref = ref false
let prev_pos : Lexing.position ref = ref Lexing.dummy_pos

type mode =
  | LEX_STRING
  | LEX_CHAR

let hijack_location lexbuf setloc saveloc =
  Sedlexing.set_position lexbuf setloc;
  (* remember to save the actual end location of the sequence so we can reset
     lexbuf to correct position at the start of next lex.*)
  sequence_lexed := true;
  prev_pos := saveloc

let store_hex hexcode =
  let code = int_of_string ("0x" ^ hexcode) in
  if Uchar.is_valid code then store_unicode code
  else Unsupported_code_point (String.uppercase_ascii hexcode) |> raise

let init () = sequence_lexed := false

let rec tokenize buf =
  if !sequence_lexed then (
    sequence_lexed := false;
    Sedlexing.set_position buf !prev_pos)
  else ();
  match%sedlex buf with
  | white_space -> tokenize buf
  | "//" -> lex_comment buf
  | "/*" -> lex_multiline_comment buf
  | number -> (
      let matched = Sedlexing.Utf8.lexeme buf in
      try INT_LIT (Int64.of_string matched)
      with Failure _ -> raise (Exceeded_maximum_int matched))
  | '-', Star white_space, "9223372036854775808" -> INT_LIT Int64.min_int
  | '\"' ->
      reset_queue ();
      let _, end_of_first = Sedlexing.lexing_positions buf in
      lex_sequence LEX_STRING end_of_first buf;
      let _, end_of_second = Sedlexing.lexing_positions buf in
      hijack_location buf end_of_first end_of_second;
      STR_LIT (Queue.copy buffer_queue)
  | '\'' -> (
      reset_queue ();
      let _, end_of_first = Sedlexing.lexing_positions buf in
      lex_sequence LEX_CHAR end_of_first buf;
      let _, end_of_second = Sedlexing.lexing_positions buf in
      hijack_location buf end_of_first end_of_second;
      (* empty chars are prohibited as well as character literals containing
         multiple characters. *)
      match Queue.length buffer_queue with
      | 0 -> raise Invalid_character
      | 1 -> CHAR_LIT (Queue.peek buffer_queue)
      | _ -> raise Invalid_character)
  | "false" -> BOOL_LIT false
  | "true" -> BOOL_LIT true
  | "boolean" -> BOOL
  | "int" -> INT
  | "char" -> CHAR
  | "type" -> TYPE
  | "void" -> VOID
  | "const" -> CONST
  | "null" -> NULL
  | "if" -> IF
  | "else" -> ELSE
  | "for" -> FOR
  | "while" -> WHILE
  | "return" -> RETURN
  | "continue" -> CONTINUE
  | "break" -> BREAK
  | "import" -> IMPORT
  | '_' -> USCORE
  | '[' -> LSBRAC
  | ']' -> RSBRAC
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LCBRAC
  | '}' -> RCBRAC
  | ';' -> SCOLON
  | '.' -> PERIOD
  | ',' -> COMMA
  | "==" -> DEQ
  | "!=" -> NEQ
  | "<=" -> LTE
  | ">=" -> GTE
  | '<' -> LT
  | '>' -> GT
  | '+' -> ADD
  | '-' -> SUB
  | '*' -> MUL
  | '/' -> DIV
  | '%' -> MOD
  | '=' -> EQ
  | '!' -> LNOT
  | "&&" -> LAND
  | "||" -> LOR
  | '~' -> BNOT
  | '&' -> BAND
  | '|' -> BOR
  | eof -> EOF
  | identifier -> ID (Sedlexing.Utf8.lexeme buf)
  | module_name ->
      (* TODO: replace with correct token *) ID (Sedlexing.Utf8.lexeme buf)
  | any ->
      let pos, _ = Sedlexing.lexing_positions buf in
      print_pos pos;
      failwith "???"
  | _ -> failwith "Unexpected characters "

and lex_comment buf =
  match%sedlex buf with
  | newline | eof -> tokenize buf
  | any -> lex_comment buf
  | _ -> failwith "impossible branch"

and lex_multiline_comment buf =
  match%sedlex buf with
  | "*/" -> tokenize buf
  | eof -> failwith "unclosed multiline comment"
  | any -> lex_multiline_comment buf
  | _ -> failwith "impossible branch"

and lex_sequence mode position buf =
  match%sedlex buf with
  | '\"' -> (
      match mode with
      | LEX_CHAR ->
          store_unicode (Uchar.of_char '\"' |> Uchar.to_int);
          lex_sequence mode position buf
      | LEX_STRING -> ())
  | '\'' -> (
      match mode with
      | LEX_CHAR -> ()
      | LEX_STRING ->
          store_unicode (Uchar.of_char '\'' |> Uchar.to_int);
          lex_sequence mode position buf)
  | eof | '\n' -> (
      let _, actual_location = Sedlexing.lexing_positions buf in
      hijack_location buf position actual_location;
      match mode with
      | LEX_STRING -> raise (Unclosed_literal "unclosed string literal")
      | LEX_CHAR -> raise (Unclosed_literal "unclosed character literal"))
  | "\\n" ->
      store_unicode (Uchar.of_char '\n' |> Uchar.to_int);
      lex_sequence mode position buf
  | "\\r" ->
      store_unicode (Uchar.of_char '\r' |> Uchar.to_int);
      lex_sequence mode position buf
  | "\\t" ->
      store_unicode (Uchar.of_char '\t' |> Uchar.to_int);
      lex_sequence mode position buf
  | "\\\"" ->
      store_unicode (Uchar.of_char '\"' |> Uchar.to_int);
      lex_sequence mode position buf
  | "\\\'" ->
      store_unicode (Uchar.of_char '\'' |> Uchar.to_int);
      lex_sequence mode position buf
  | "\\\\" ->
      store_unicode (Uchar.of_char '\\' |> Uchar.to_int);
      lex_sequence mode position buf
  | "\\u", hex, hex, hex, hex ->
      let hexcode = String.sub (Sedlexing.Utf8.lexeme buf) 2 4 in
      store_hex hexcode;
      lex_sequence mode position buf
  | "\\x{", Rep (hex, 1 .. 6), "}" ->
      let s = Sedlexing.Utf8.lexeme buf in
      (* ignore the start \x{ and total ignoring 4 characters \,x,{,} *)
      let hexcode = String.sub s 3 (String.length s - 4) in
      store_hex hexcode;
      lex_sequence mode position buf
  | '\\', any -> raise (Invalid_escape (Sedlexing.Utf8.lexeme buf))
  | any ->
      store_unicode (Uchar.to_int (Sedlexing.lexeme_char buf 0));
      lex_sequence mode position buf
  | _ -> failwith "impossible branch"
