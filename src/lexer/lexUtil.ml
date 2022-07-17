open Parse.Parser

(** [string_of_uchar uc] is the escaped string representation of an unicode
    character [uc]. *)
let string_of_uchar uchar =
  let code = Uchar.to_int uchar in
  if code >= 128 then Printf.sprintf "\\x{%06x}" code
  else Uchar.to_char uchar |> String.make 1 |> String.escaped

let string_of_token = function
  | WHILE -> "while"
  | VOID -> "void"
  | SUB -> "-"
  | STR_LIT int_queue ->
      let buffer = Buffer.create 16 in
      Queue.iter
        (fun code ->
          Uchar.of_int code |> string_of_uchar |> Buffer.add_string buffer)
        int_queue;
      let s = Buffer.contents buffer in
      Buffer.clear buffer;
      "string " ^ Printf.sprintf "\"%s\"" s
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
  | ID id -> "id " ^ id
  | GTE -> ">="
  | GT -> ">"
  | FOR -> "for"
  | EQ -> "="
  | EOF -> "EOF"
  | ELSE -> "else"
  | DIV -> "/"
  | DEQ -> "=="
  | CINT -> "(int)"
  | CHAR_LIT code -> "character " ^ string_of_uchar (Uchar.of_int code)
  | CHAR -> "char"
  | CCHAR -> "(char)"
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

open Lexer

exception Lexical_error of int * int * string

(** [get_line_col pos] is a lexical position's corresponding text file line and
    column numbers. *)
let get_line_col (position : Lexing.position) =
  (position.pos_lnum, position.pos_cnum - position.pos_bol + 1)

(** [lex_error] raises a [Lexical_error (l, c, s)] where [l] and [c] specifies
    the line and column of the error and [s] details the error. *)
let lex_error lexbuf msg =
  let position, _ = Sedlexing.lexing_positions lexbuf in
  let l, c = get_line_col position in
  raise (Lexical_error (l, c, msg))

let lex_aux ic lexical_action error_action =
  let sedlexbuf = Sedlexing.Utf8.from_channel ic in
  let terminate_lex msg =
    error_action sedlexbuf msg;
    lex_error sedlexbuf msg
  in
  let rec loop lexbuf =
    let token =
      try Lexer.tokenize lexbuf with
      | Exceeded_maximum_int _ -> terminate_lex "integer number too large"
      | Invalid_escape _ -> terminate_lex "invalid character escape"
      | Unclosed_literal s -> terminate_lex s
      | Unsupported_code_point s ->
          terminate_lex (Printf.sprintf "%s is not a supported code point" s)
    in
    let position, _ = Sedlexing.lexing_positions lexbuf in
    match token with
    | EOF -> ()
    | _ ->
        lexical_action token position;
        loop lexbuf
  in
  loop sedlexbuf

let do_nothing _ _ = ()
let lex_no_output ic = lex_aux ic do_nothing do_nothing

let lex_with_output ic oc =
  lex_aux ic
    (fun token position ->
      let l, c = get_line_col position in
      Printf.fprintf oc "%d:%d %s\n" l c (string_of_token token))
    (fun lexbuf error_msg ->
      let position, _ = Sedlexing.lexing_positions lexbuf in
      let l, c = get_line_col position in
      Printf.fprintf oc "%d:%d error:%s\n" l c error_msg)
