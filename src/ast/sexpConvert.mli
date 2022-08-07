(** This module can be used to convert AST components to S-expressions*)

open Node

type sexp =
  | Atom of string
  | List of sexp list

val sexp_of_expr : expr -> sexp
val sexp_of_stmt : stmt -> sexp
val sexp_of_file : file -> sexp
val print_sexp : Util.SexpPrinter.printer -> sexp -> unit
