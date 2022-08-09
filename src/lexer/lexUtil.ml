open Parse
open Lexer

type error = Lexing.position * string

let lex_aux sedlexbuf lexical_action error_action : (unit, error) result =
  let lexer = make_lexer sedlexbuf in
  let terminate_lex msg =
    error_action lexer msg;
    Stdlib.Error (Lexer.get_position lexer, msg)
  in
  let rec loop lexer =
    match Lexer.tokenize lexer with
    | token -> begin
        let position = Lexer.get_position lexer in
        match token with
        | EOF -> Ok ()
        | _ ->
            lexical_action position token;
            loop lexer
      end
    | exception Exceeded_maximum_int -> terminate_lex "integer number too large"
    | exception Invalid_escape s ->
        terminate_lex ("invalid character escape " ^ s)
    | exception Unclosed_literal s -> terminate_lex s
    | exception Unsupported_code_point s ->
        terminate_lex (Printf.sprintf "%s is not a supported code point" s)
    | exception Invalid_character -> terminate_lex "invalid character literal"
    | exception Illegal_character -> terminate_lex "illegal character"
  in
  loop lexer

let lex_no_output lexbuf =
  let do_nothing _ _ = () in
  lex_aux lexbuf do_nothing do_nothing

let lex_with_output lexbuf fmt =
  lex_aux lexbuf
    (fun position token ->
      let l, c = Util.Position.coord_of_pos position in
      Format.fprintf fmt "%d:%d %s\n" l c (string_of_token token))
    (fun lexer error_msg ->
      let position = Lexer.get_position lexer in
      let l, c = Util.Position.coord_of_pos position in
      Format.fprintf fmt "%d:%d error:%s\n" l c error_msg)
