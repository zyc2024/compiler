open Parser
open Ast

type token_generator = unit -> token * Lexing.position * Lexing.position
type error = Lexing.position * string

(** [string_of_unicode c] is the escaped string representation of an unicode
    character whose unicode representation code is [c]. *)
let string_of_unicode code =
  if code >= 128 then
    Printf.sprintf "{%06x}" code |> String.uppercase_ascii |> ( ^ ) "\\x"
  else Uchar.to_char (Uchar.of_int code) |> String.make 1 |> String.escaped

let string_of_token = function
  | WHILE -> "while"
  | VOID -> "void"
  | SUB -> "-"
  | STR_LIT int_queue ->
      let buffer = Buffer.create 16 in
      Queue.iter
        (fun code -> string_of_unicode code |> Buffer.add_string buffer)
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
  | ID id | MODULE_ID id -> "id " ^ id
  | GTE -> ">="
  | GT -> ">"
  | FOR -> "for"
  | EQ -> "="
  | EOF -> "EOF"
  | ELSE -> "else"
  | DIV -> "/"
  | DEQ -> "=="
  | CHAR_LIT code -> "character " ^ string_of_unicode code
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

(* TODO: replace return types with proper AST top level node. *)
let parse_aux (generator : token_generator) action =
  (* use the token generator provided but modifying these values so we know what
     to output on error. // this wouldn't be necessary if we have a complete
     Incremental engine setup.

     Subject to implementation change.*)
  let token_lexed = ref EOF in
  let token_pos = ref Lexing.dummy_pos in
  (* hacking the generator by recording its output value and then returning to
     caller (parser) those values. *)
  let lexer () =
    let t, pos1, pos2 = generator () in
    token_lexed := t;
    token_pos := pos1;
    (t, pos1, pos2)
  in
  let parser = MenhirLib.Convert.Simplified.traditional2revised parse_stmt in
  try
    let ast_node = parser lexer in
    action ast_node;
    Ok ast_node
  with Parser.Error ->
    Error
      ( !token_pos,
        Printf.sprintf "unexpected token %s" (string_of_token !token_lexed) )

let parse (generator : token_generator) : (AstNode.stmt_node, error) result =
  parse_aux generator (fun _ -> ())

(* TODO *)
let parse_with_output (generator : token_generator) oc =
  parse_aux generator (fun _ ->
      let _ = Format.formatter_of_out_channel oc in
      ())
