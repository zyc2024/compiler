include AstNode

(** This module can be used to convert AST components to S-expressions*)
module SexpConvert = struct
  type sexp = Core.Sexp.t

  let sexp_of_expr (e : expr) : sexp = AstUtil.sexp_of_expr e
  let sexp_of_stmt (s : stmt) : sexp = AstUtil.sexp_of_stmt s
  let sexp_of_file (f : file) : sexp = AstUtil.sexp_of_file f

  let print_sexp (fmt : Format.formatter) (s : sexp) : unit =
    AstUtil.print_sexp fmt s
end
