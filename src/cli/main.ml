(* open Lex.ExampleModule

open Ast *)
(* 
let () = Lex.ExampleModule.do_something *)

(* let () = print_endline "Hello, World!"

(* this is an example of using the AST defined types *)
let e = BinopExpr(ADD , IntLiteral 1 , Strliteral "h")

let () = print_endline (string_of_int (sum 3 4)) *)

(* let f = open_in "t.txt" in 
    let tl = Lex.get_token_list (Lexing.from_channel f) in
        let ptl = function
            | (tt, p) :: t -> 
        ;; *)



(* 
let f = open_in "t.txt" in 
    Lex.LexUtil.output_tokens f stdout ;;  *)

(* let f = open_in "t.txt" in 
let flb = Lexing.from_channel f in
Parse.Parser.main Lex.Lexer.evo_lex flb;; *)

open Parse.ParseUtil

let () = parse_with_first_token_fed Lex.Lexer.evo_lex (open_in "t.txt") stdout 