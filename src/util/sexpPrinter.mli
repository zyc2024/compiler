(** A simple S-expression Printer *)

(** [print_atom fmt atom] appends the string [atom] to the formatter [fmt].*)
val print_atom : Format.formatter -> string -> unit

(** [start_list fmt ()] initializes a new printing box/sequence. *)
val start_list : Format.formatter -> unit -> unit

(** [end_list fmt ()] closes the most recently opened box/sequence. *)
val end_list : Format.formatter -> unit -> unit
