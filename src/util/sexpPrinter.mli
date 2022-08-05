(** A simple S-expression Printer *)

type printer

val make_printer : Format.formatter -> printer

(** [print_atom p atom] appends the string [atom] to the end of printer [p].*)
val print_atom : printer -> string -> unit

(** [start_list p ()] initializes a new printing box/sequence. *)
val start_list : printer -> unit -> unit

(** [end_list p ()] closes the most recently opened box/sequence. *)
val end_list : printer -> unit -> unit
