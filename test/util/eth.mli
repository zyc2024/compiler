(** Eth is the Evo testing helper. *)

(** [compare file_name ic1 ic2] is [Ok ()] when the two input streams contain
    the same data and [Error s] when there is a mismatch with [s] detailing the
    error with format
    [Mismatch detected at line %d of file <file_name> expected: %s found: %s] *)
val compare :
  string -> expected:in_channel -> input:in_channel -> (unit, string) Result.t

(** [compare_sexp file_name ic1 ic2] is [OK ()] when the two S-expressions
    parsed from [ic1] and [ic2] are identical and [Error s] when there is a
    token mismatch with [s] detailing the error with format
    [Mismatch detected at %d:%d of file <file_name> expected: <token> found: <token>]*)
val compare_sexp :
  string -> expected:in_channel -> input:in_channel -> (unit, string) Result.t