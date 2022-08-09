type error = Lexing.position * string

(** [lex lexbuf] is [Ok ()] if lexical analysis is successfully completed on the
    lexer buffer [lexbuf], otherwise [Error (position, message)]. *)
val lex_no_output : Sedlexing.lexbuf -> (unit, error) result

(** [lex_with_output lexbuf fmt] is [lex lexbuf] but prints the tokens to the
    formatter [fmt]. Printing stops at EOF or after a lexical error is
    encountered in which case the error is recorded in [Error e]. *)
val lex_with_output :
  Sedlexing.lexbuf -> Format.formatter -> (unit, error) result
