(** A simple S-expression Printer *)

(** [get_current_formatter ()] is the formatter in use. The default formatter is
    [Format.std_formatter]. *)
val get_current_formatter : unit -> Format.formatter

(** [set_formatter fmt] replaces the current formatter with [fmt]. Note that for
    safety reasons, the current formatter should be flushed beforehand. *)
val set_formatter : Format.formatter -> unit

(** [flush_contents ()] outputs the S-expression string into the formatter's
    output device. *)
val flush_contents : unit -> unit

(** [print_atom atom] appends the string [atom] to the current formatter.*)
val print_atom : string -> unit

(** [start_list ()] initializes a new printing box/sequence. *)
val start_list : unit -> unit

(** [end_list ()] closes the most recently opened box/sequence. *)
val end_list : unit -> unit