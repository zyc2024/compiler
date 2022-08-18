open Parser
module Interp = Parser.MenhirInterpreter

type token_info = token * Lexing.position * Lexing.position
type token_generator = unit -> token_info

let string_of_token = function
  | WHILE -> "while"
  | VOID -> "void"
  | SUB -> "-"
  | STR_LIT int_queue ->
      Printf.sprintf "string \"%s\"" (Util.Unicode.string_of_intq int_queue)
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
  | CHAR_LIT code ->
      Printf.sprintf "character \'%s\'" (Util.Unicode.string_of_unicode code)
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

let name_of_token t =
  match t with
  | STR_LIT _ -> "string"
  | INT_LIT _ -> "integer"
  | ID _ -> "identifier"
  | MODULE_ID _ -> "type identifier"
  | CHAR_LIT _ -> "character"
  | USCORE -> "underscore"
  | WHILE | VOID | BOOL_LIT _ | NULL | INT | IMPORT | FOR | RETURN | CONTINUE
  | EOF | IF | ELSE | BREAK | CHAR | BOOL | CONST ->
      string_of_token t
  | TYPE -> "keyword " ^ string_of_token t
  | SCOLON | RSBRAC | RPAREN | RCBRAC | PERIOD | NEQ | MUL | MOD | LTE | LT
  | LSBRAC | LPAREN | LOR | LNOT | LCBRAC | LAND | GTE | GT | EQ | DIV | DEQ
  | ADD | SUB | BNOT | BAND | BOR | COMMA | COLON ->
      Printf.sprintf "'%s'" (string_of_token t)

let parse_aux incrementer (generator : token_generator) =
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
          Format.sprintf "unexpected %s" (name_of_token current_token)
        in
        Stdlib.Error (current_pos, msg)
    | Interp.Accepted v -> Ok v
    | Interp.Rejected -> assert false
  in
  try Lexing.(loop dummy_pos EOF (incrementer dummy_pos)) with
  | SyntaxError.Not_a_statement pos -> Stdlib.Error (pos, "not a statement")
  | SyntaxError.Not_a_location pos ->
      Stdlib.Error (pos, "a value is not a variable/location")
  | SyntaxError.Not_a_function_call pos ->
      Stdlib.Error (pos, "a function call is expected")

let parse (generator : token_generator) mode =
  match mode with
  | `Module -> parse_aux Parser.Incremental.parse_module generator
  | `Interface -> failwith "TODO: support interface parsing"

(* type state = { checkpoint : Ast.file Interp.checkpoint; current_token :
   token; current_position : Lexing.position; }

   let init_module_state = { checkpoint = Parser.Incremental.parse_module
   Lexing.dummy_pos; current_token = EOF; current_position = Lexing.dummy_pos; }

   let consume_token state (token, startp, endp) = let rec consume checkpoint =
   match checkpoint with | Interp.InputNeeded _env -> { checkpoint =
   Interp.offer checkpoint (token, startp, endp); current_position = startp;
   current_token = token; } | Interp.Shifting _ | Interp.AboutToReduce _ ->
   consume (Interp.resume checkpoint) | Interp.HandlingError _env -> state |
   Interp.Accepted _ -> state | Interp.Rejected -> state in consume
   state.checkpoint

   let rec result_of_state state = let open Stdlib in match state.checkpoint
   with | Interp.InputNeeded _env -> Error (state.current_position, "input
   needed") | Interp.Shifting _ | Interp.AboutToReduce _ -> result_of_state {
   state with checkpoint = Interp.resume state.checkpoint } |
   Interp.HandlingError _ | Interp.Rejected -> Error ( state.current_position,
   "unexpected " ^ name_of_token state.current_token ) | Interp.Accepted ast ->
   Ok ast *)
