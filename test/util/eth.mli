(** Eth is the Evo testing helper. *)

(** [compare file_name ic1 ic2] is [Ok ()] when the two input streams contain
    the same data and [Error s] when there is a mismatch with [s] detailing the
    error with format
    [Mismatch detected at line %d of file <file_name> expected: %s found: %s] *)
val compare : string -> in_channel -> in_channel -> (unit, string) Result.t
