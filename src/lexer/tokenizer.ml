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

open Parse

exception Lexical_error of Lexing.position * string

type t = {
  lexbuf : Sedlexing.lexbuf;
  buffer_queue : int Queue.t;
  sequence_lexed : bool ref;
  prev_pos : Lexing.position ref;
}

let get_positions lexer = Sedlexing.lexing_positions lexer.lexbuf

let get_position lexer =
  let pos, _ = get_positions lexer in
  pos

let reset_queue lexer = Queue.clear lexer.buffer_queue

(* let print_pos (pos : Lexing.position) = print_endline (Printf.sprintf "%d:%d"
   pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)) *)

type mode =
  | LEX_STRING
  | LEX_CHAR

(** [hijack_location lexer override saved] updates the lexer's lexbuf position
    to the position [override] and remembers the [saved] position so that on the
    next tokenization, the lexer's position is corrected. *)
let hijack_location lexer setloc saveloc =
  Sedlexing.set_position lexer.lexbuf setloc;
  (* remember to save the actual end location of the sequence so we can reset
     lexbuf to correct position at the start of next lex.*)
  lexer.sequence_lexed := true;
  lexer.prev_pos := saveloc

(** [store_unicode lexer code] adds [code] to the end of the lexer's unicode
    buffer. *)
let store_unicode lexer code = Queue.add code lexer.buffer_queue

(** [store_hex lexer hex] converts [hex] into its decimal representation and
    adds to the lexer's unicode buffer. *)
let store_hex lexer hexcode =
  let code = int_of_string ("0x" ^ hexcode) in
  let code_10ffff = 1114111 in
  if code <= code_10ffff then store_unicode lexer code
  else
    raise
      (Lexical_error
         ( get_position lexer,
           Printf.sprintf "%s is not a supported code point"
             (String.uppercase_ascii hexcode) ))

let make_lexer lexbuf =
  {
    lexbuf;
    buffer_queue = Queue.create ();
    sequence_lexed = ref false;
    prev_pos = ref Lexing.dummy_pos;
  }

let rec tokenize lexer =
  let buf = lexer.lexbuf in
  if !(lexer.sequence_lexed) then begin
    lexer.sequence_lexed := false;
    Sedlexing.set_position buf !(lexer.prev_pos)
  end
  else ();
  match%sedlex buf with
  | white_space -> tokenize lexer
  | "//" -> lex_comment lexer buf
  | "/*" -> lex_multiline_comment lexer buf
  | number -> (
      let matched = Sedlexing.Utf8.lexeme buf in
      try INT_LIT (Int64.of_string matched)
      with Failure _ ->
        raise (Lexical_error (get_position lexer, "integer number too large")))
  | '-', Star white_space, "9223372036854775808" -> INT_LIT Int64.min_int
  | '\"' ->
      reset_queue lexer;
      let _, end_of_first = Sedlexing.lexing_positions buf in
      lex_sequence LEX_STRING end_of_first lexer buf;
      let _, end_of_second = Sedlexing.lexing_positions buf in
      hijack_location lexer end_of_first end_of_second;
      STR_LIT (Queue.copy lexer.buffer_queue)
  | '\'' -> (
      reset_queue lexer;
      let _, end_of_first = Sedlexing.lexing_positions buf in
      lex_sequence LEX_CHAR end_of_first lexer buf;
      let _, end_of_second = Sedlexing.lexing_positions buf in
      hijack_location lexer end_of_first end_of_second;
      (* empty chars are prohibited as well as character literals containing
         multiple characters. *)
      match Queue.length lexer.buffer_queue with
      | 0 ->
          raise
            (Lexical_error (get_position lexer, "invalid character literal"))
      | 1 -> CHAR_LIT (Queue.peek lexer.buffer_queue)
      | _ ->
          raise
            (Lexical_error (get_position lexer, "invalid character literal")))
  | "false" -> BOOL_LIT false
  | "true" -> BOOL_LIT true
  | "bool" -> BOOL
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
  | ':' -> COLON
  | eof -> EOF
  | identifier -> ID (Sedlexing.Utf8.lexeme buf)
  | module_name -> MODULE_ID (Sedlexing.Utf8.lexeme buf)
  | any -> raise (Lexical_error (get_position lexer, "illegal character"))
  | _ -> failwith "Only UTF-8 scalar values are allowed in source file"

and lex_comment lexer buf =
  match%sedlex buf with
  | newline | eof -> tokenize lexer
  | any -> lex_comment lexer buf
  | _ -> failwith "Only UTF-8 scalar values are allowed in source file"

and lex_multiline_comment lexer buf =
  match%sedlex buf with
  | "*/" -> tokenize lexer
  | eof -> EOF
  | any -> lex_multiline_comment lexer buf
  | _ -> failwith "Only UTF-8 scalar values are allowed in source file"

and lex_sequence mode position lexer buf =
  match%sedlex buf with
  | '\"' -> (
      match mode with
      | LEX_CHAR ->
          store_unicode lexer (Uchar.of_char '\"' |> Uchar.to_int);
          lex_sequence mode position lexer buf
      | LEX_STRING -> ())
  | '\'' -> (
      match mode with
      | LEX_CHAR -> ()
      | LEX_STRING ->
          store_unicode lexer (Uchar.of_char '\'' |> Uchar.to_int);
          lex_sequence mode position lexer buf)
  | eof -> begin
      let open Lexing in
      let end_loc = { position with pos_cnum = position.pos_cnum - 1 } in
      hijack_location lexer end_loc position;
      match mode with
      | LEX_STRING ->
          raise (Lexical_error (get_position lexer, "unclosed string literal"))
      | LEX_CHAR ->
          raise
            (Lexical_error (get_position lexer, "unclosed character literal"))
    end
  | '\n' -> (
      let _, actual_location = Sedlexing.lexing_positions buf in
      hijack_location lexer position actual_location;
      match mode with
      | LEX_STRING ->
          raise (Lexical_error (get_position lexer, "unclosed string literal"))
      | LEX_CHAR ->
          raise
            (Lexical_error (get_position lexer, "unclosed character literal")))
  | "\\n" ->
      store_unicode lexer (Uchar.of_char '\n' |> Uchar.to_int);
      lex_sequence mode position lexer buf
  | "\\r" ->
      store_unicode lexer (Uchar.of_char '\r' |> Uchar.to_int);
      lex_sequence mode position lexer buf
  | "\\t" ->
      store_unicode lexer (Uchar.of_char '\t' |> Uchar.to_int);
      lex_sequence mode position lexer buf
  | "\\\"" ->
      store_unicode lexer (Uchar.of_char '\"' |> Uchar.to_int);
      lex_sequence mode position lexer buf
  | "\\\'" ->
      store_unicode lexer (Uchar.of_char '\'' |> Uchar.to_int);
      lex_sequence mode position lexer buf
  | "\\\\" ->
      store_unicode lexer (Uchar.of_char '\\' |> Uchar.to_int);
      lex_sequence mode position lexer buf
  | "\\u", hex, hex, hex, hex ->
      let hexcode = String.sub (Sedlexing.Utf8.lexeme buf) 2 4 in
      store_hex lexer hexcode;
      lex_sequence mode position lexer buf
  | "\\x{", Rep (hex, 1 .. 6), "}" ->
      let s = Sedlexing.Utf8.lexeme buf in
      (* ignore the start \x{ and total ignoring 4 characters \,x,{,} *)
      let hexcode = String.sub s 3 (String.length s - 4) in
      store_hex lexer hexcode;
      lex_sequence mode position lexer buf
  | '\\', any ->
      raise
        (Lexical_error
           ( get_position lexer,
             "invalid character escape " ^ Sedlexing.Utf8.lexeme buf ))
  | any ->
      store_unicode lexer (Uchar.to_int (Sedlexing.lexeme_char buf 0));
      lex_sequence mode position lexer buf
  | _ -> failwith "Only UTF-8 scalar values are allowed in source file"
