(** A simple S-expression Printer *)

type printer

val make_printer : Format.formatter -> printer

(** [make_printer_custom fmt spaces] is [make_printer fmt] with number of spaces
    used in indentation equal to [spaces]. The default case is [2]. *)
val make_printer_custom : Format.formatter -> int -> printer

(** [print_atom p atom] appends the string [atom] to the end of printer [p].*)
val print_atom : printer -> string -> unit

(** [start_list p ()] initializes a new printing box/sequence. *)
val start_list : printer -> unit -> unit

(** [end_list p ()] closes the most recently opened box/sequence. *)
val end_list : printer -> unit -> unit
