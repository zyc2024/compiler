(** Eth is the Evo testing helper. *)

(** [compare file_name ic1 ic2] is [Ok ()] when the two input streams contain
    the same data and [Error s] when there is a mismatch with [s] detailing the
    error with format
    [Mismatch detected at line %d of file <file_name> expected: %s found: %s] *)
val compare : string -> in_channel -> in_channel -> (unit, string) Result.t

(** [tokenized_compare file_name tokenizer eqfn end_token str_of_tok ic1 ic2] is
    [Ok()] when the two input streams contain streams of equivalent tokens
    according to the provided equality function [eqfn]. [tokenized_compare]
    returns [Error s] when the token streams differ according to the provided
    equality function or when one stream scans [end_token] before the other
    stream. The Error message details the mismatch as follows:
    [Mismatch detected at line %d of file <file_name>: expected %s found %s]
    where %s are string representations of the tokens as provided by
    [str_of_tok] *)
val tokenized_compare :
  string ->
  (Lexing.lexbuf -> 'a) ->
  ('a -> 'a -> bool) ->
  'a ->
  ('a -> string) ->
  in_channel ->
  in_channel ->
  (unit, string) Result.t
