open Parser
module Interp = Parser.MenhirInterpreter

type token_generator = unit -> token * Lexing.position * Lexing.position

(** [string_of_unicode c] is the escaped string representation of an unicode
    character whose unicode representation code is [c]. *)
(* let string_of_unicode code = if code >= 128 then Printf.sprintf "{%06x}" code
   |> String.uppercase_ascii |> ( ^ ) "\\x" else Uchar.to_char (Uchar.of_int
   code) |> String.make 1 |> String.escaped *)

let string_of_token = function
  | WHILE -> "while"
  | VOID -> "void"
  | SUB -> "-"
  | STR_LIT int_queue ->
      "string "
      ^ Printf.sprintf "\"%s\"" (Util.Unicode.string_of_intq int_queue)
  | SCOLON -> ";"
  | RSBRAC -> "]"
  | RPAREN -> ")"
  | RCBRAC -> "}"
  | PERIOD -> "."
  | NULL -> "null"
  | NEQ -> "!="
  | MUL -> "*"
  | MOD -> "%"
  | LTE -> "<="
  | LT -> "<"
  | LSBRAC -> "["
  | LPAREN -> "("
  | LOR -> "||"
  | LNOT -> "!"
  | LCBRAC -> "{"
  | LAND -> "&&"
  | INT_LIT i -> "integer " ^ Int64.to_string i
  | INT -> "int"
  | IMPORT -> "import"
  | IF -> "if"
  | ID id | MODULE_ID id -> "id " ^ id
  | GTE -> ">="
  | GT -> ">"
  | FOR -> "for"
  | EQ -> "="
  | EOF -> "EOF"
  | ELSE -> "else"
  | DIV -> "/"
  | DEQ -> "=="
  | CHAR_LIT code -> "character " ^ Util.Unicode.string_of_unicode code
  | CHAR -> "char"
  | BOOL_LIT b -> string_of_bool b
  | BOOL -> "bool"
  | ADD -> "+"
  | RETURN -> "return"
  | CONTINUE -> "continue"
  | BREAK -> "break"
  | BNOT -> "~"
  | BAND -> "&"
  | BOR -> "|"
  | COMMA -> ","
  | TYPE -> "type"
  | CONST -> "const"
  | USCORE -> "_"
  | COLON -> ":"

let parse_aux incrementer (generator : token_generator) action error_action =
  let rec loop current_pos current_token checkpoint =
    match checkpoint with
    | Interp.InputNeeded _env ->
        let token, startp, endp = generator () in
        let checkpoint = Interp.offer checkpoint (token, startp, endp) in
        loop startp token checkpoint
    | Interp.Shifting _ | Interp.AboutToReduce _ ->
        let checkpoint = Interp.resume checkpoint in
        loop current_pos current_token checkpoint
    | Interp.HandlingError _env ->
        let msg =
          Format.sprintf "unexpected token %s" (string_of_token current_token)
        in
        let err = (current_pos, msg) in
        error_action err;
        Stdlib.Error err
    | Interp.Accepted v ->
        action v;
        Ok v
    | Interp.Rejected -> assert false
  in
  try Lexing.(loop dummy_pos EOF (incrementer dummy_pos)) with
  (* wondering if there's a better alternative (support from menhir) but halting
     the parser like this is probably the approach for now. *)
  | SyntaxError.Not_a_statement pos ->
      let err = (pos, "not a statement") in
      error_action err;
      Stdlib.Error err
  | SyntaxError.Not_a_location pos ->
      let err = (pos, "a value is not a variable/location") in
      error_action err;
      Stdlib.Error err

let parse (generator : token_generator) mode =
  let no_action _ = () in
  let _ = match mode with `Module -> () | `Interface -> () in
  parse_aux Parser.Incremental.parse_module generator no_action no_action

let parse_with_output (generator : token_generator) mode fmt =
  match mode with
  | `Module ->
      parse_aux Parser.Incremental.parse_module generator
        (fun file -> Ast.SexpConvert.(print_sexp fmt (sexp_of_file file)))
        (fun (pos, msg) ->
          let l, c = Util.Position.coord_of_pos pos in
          Format.pp_print_string fmt (Format.sprintf "%d:%d error:%s" l c msg))
  | `Interface -> failwith "TODO: support interface parsing"
