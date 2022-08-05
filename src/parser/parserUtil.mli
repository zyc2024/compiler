open Parser

(** the abstract type of a token generator which produces tokens along with
    their positions to be consumed by a parser (ie: Menhir). *)
type token_generator = unit -> token * Lexing.position * Lexing.position

(** [string_of_token t] is the string representation of the given token [t]. *)
val string_of_token : token -> string

(** [parse generator mode] is [Ok file] if the generator produces a token stream
    with a matching AST structure, otherwise [Error e].

    Note: the generator's tokens will be consumed and the functionality of
    putting the tokens back is dependent on the construction of [generator]:
    such feature is not provided here.*)
val parse :
  token_generator ->
  [< `Module | `Interface ] ->
  (Ast.file, Lexing.position * string) Stdlib.result

(** [parse_with_output generator mode fmt] is [parse generator mode] but with an
    S-expression printed to [fmt] when parsing succeeds. *)
val parse_with_output :
  token_generator ->
  [< `Module | `Interface ] ->
  Format.formatter ->
  (Ast.file, Lexing.position * string) Stdlib.result
