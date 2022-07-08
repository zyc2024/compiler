exception MissingPositionalArgument of string
exception IncorrectFileExtension of string
exception UnsupportedFlag of string

type spec

(* [make_spec name description arg_name ?alternatives verifier] is a spec with
   target [name] whose positional argument is checked by being applied as
   argument to [verifier]. The [alternatives] argument provides additional names
   which are equivalent to [name].*)
val make_spec :
  string ->
  ?alternatives:string list ->
  description:string ->
  arg:string ->
  (string -> unit) ->
  spec

(* [make_spec_no_arg name description] is a spec with the target [name] and
   takes no positional argument. *)
val make_spec_no_arg : string -> ?alternatives:string list -> string -> spec

(* [no_verify] is a dummy verifier that does nothing.*)
val no_verify : string -> unit

(* [description spec] is the description of the given spec*)
val description : spec -> string

(* [name spec] is the name of the provided spec. If the spec has alternative
   names, see [names spec]*)
val name : spec -> string

(* [names spec] is the list of all the names associated with the provided
   spec. *)
val names : spec -> string list

(* [compare_names spec1 spec2] is the comparison result from comparing the two
   given specs with respect to their main name. The comparison ignores the
   number of dashes in the flag names.*)
val compare_names : spec -> spec -> int

(* [spec_usage spec] is the display string containing the usage format of the
   flag. Example: --classpath <path> *)
val spec_usage : spec -> string

(* the abstract type representing the parsing result (flags, their assigned
   values, and input files) *)
type result

(* [parse args specs] is the result containing the enabled specs in the order
   provided through the command line. Any required arguments to a particular
   invoked flag is also saved. Throws MissingPositionalArgument in the case that
   a flag requires an argument and such argument is not provided. Throws
   UnsupportedFlag if a flag is passed through the command line but the CLI
   specification does not support such flag.*)
val parse : string list -> spec list -> result

(* [flag_and_args parsed] is the list of tuple elements (flag, value) where the
   value associated with the flag should be ignored if the flag does not take a
   positional argument. The list is ordered with respect to the order of the
   argument passed to the CLI. Consequentely, the same flag may appear multiple
   times in the list.*)
val flag_and_args : result -> (string * string) list

(* [files parsed exts] is the list of input files to the process. Throws
   IncorrectFileExtension if any of the file arguments does not have an
   extension listed in [exts]. *)
val files : result -> string list -> string list
