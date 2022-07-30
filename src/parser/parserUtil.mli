open Parser
open Ast

(** the abstract type of a token generator which produces tokens along with
    their positions to be consumed by a parser (ie: Menhir). *)
type token_generator = unit -> token * Lexing.position * Lexing.position

(** the format of an error consists of its position and description. *)
type error = Lexing.position * string

(** [string_of_token t] is the string representation of the given token [t]. *)
val string_of_token : token -> string

(** [parse generator] is [Ok ()] if the generator produces a token stream with a
    matching AST structure, otherwise [Error e].

    Note: the generator's tokens will be consumed and the functionality of
    putting the tokens back is dependent on the construction of [generator]:
    such feature is not provided here.*)
val parse : token_generator -> (AstNode.stmt_node, error) Stdlib.result

(** [parse_with_output generator oc] is [parse generator] but with an
    S-expression printed to [oc] when parsing succeeds. *)
val parse_with_output :
  token_generator -> out_channel -> (AstNode.stmt_node, error) Stdlib.result
