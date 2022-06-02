open Lex.ExampleModule

open Ast

let () = print_endline "Hello, World!"

(* this is an example of using the AST defined types *)
let e = BinopExpr(ADD , IntLiteral 1 , Strliteral "h")

let () = print_endline (string_of_int (sum 3 4))
