open Parse
open Lexer

exception Lexical_error of Lexing.position * string

let lex_aux ic lexical_action error_action =
  let sedlexbuf = Sedlexing.Utf8.from_channel ic in
  let lexer : Lexer.t = make_lexer sedlexbuf in
  let terminate_lex msg =
    error_action lexer msg;
    raise (Lexical_error (Lexer.get_position lexer, msg))
  in
  let rec loop lexer =
    let token =
      try Lexer.tokenize lexer with
      | Exceeded_maximum_int -> terminate_lex "integer number too large"
      | Invalid_escape s -> terminate_lex ("invalid character escape " ^ s)
      | Unclosed_literal s -> terminate_lex s
      | Unsupported_code_point s ->
          terminate_lex (Printf.sprintf "%s is not a supported code point" s)
      | Invalid_character -> terminate_lex "invalid character literal"
      | Illegal_character -> terminate_lex "illegal character"
    in
    let position = Lexer.get_position lexer in
    match token with
    | EOF -> ()
    | _ ->
        lexical_action token position;
        loop lexer
  in
  loop lexer

let lex_no_output ic =
  let do_nothing _ _ = () in
  lex_aux ic do_nothing do_nothing

let lex_with_output ic oc =
  lex_aux ic
    (fun token position ->
      let l, c = Util.Position.coord_of_pos position in
      Printf.fprintf oc "%d:%d %s\n" l c (string_of_token token))
    (fun lexer error_msg ->
      let position = Lexer.get_position lexer in
      let l, c = Util.Position.coord_of_pos position in
      Printf.fprintf oc "%d:%d error:%s\n" l c error_msg)
