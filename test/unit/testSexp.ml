(** This test script verifies the conversion from AST syntax trees to
    S-expressions. *)

open OUnit2
open Ast

module Helper = struct
  let buffer = Buffer.create 256

  (** [pp_sexp s] converts [s] into a pretty formatted string. *)
  let pp_sexp sexp =
    Buffer.reset buffer;
    let fmt = Format.formatter_of_buffer buffer in
    let printer = Util.SexpPrinter.make_printer fmt in
    SexpConvert.(print_sexp printer sexp);
    Format.pp_print_flush fmt ();
    Buffer.contents buffer

  let make_int i = IntLiteral (Int64.of_int i)
  let make_int_node i = (Lexing.dummy_pos, make_int i)

  let make_test name expected func input ~printer =
    name >:: fun _ -> assert_equal expected (func input) ~printer
end

open Helper

(* for internal use only: given a list of anything, turn them into nodes by
   associating each list element with dummypos*)
let nodify_elements l = List.map (fun x -> (Lexing.dummy_pos, x)) l

let expr_tests =
  (* [make name expected input] runs an Ounit test by converting the input to an
     sexp and comparing it against the given sexp [expected]. *)
  let make name expected input_expr =
    make_test name expected SexpConvert.sexp_of_expr input_expr ~printer:pp_sexp
  in
  let make_binop name op string_of_op =
    make name
      (List [ Atom string_of_op; Atom "1"; Atom "2" ])
      (BinopExpr (op, make_int_node 1, make_int_node 2))
  in
  let intq_of_lst (lst : int list) =
    let q = Queue.create () in
    List.iter (fun e -> Queue.add e q) lst;
    q
  in
  let hello_world = [ 104; 101; 108; 108; 111; 32; 119; 111; 114; 108; 100 ] in
  [
    make "max int" (Atom "9223372036854775807") (IntLiteral Int64.max_int);
    make "min int" (Atom "-9223372036854775808") (IntLiteral Int64.min_int);
    make "unary not"
      (List [ Atom "!"; Atom "true" ])
      (UnaryExpr (Not, (Lexing.dummy_pos, BoolLiteral true)));
    make "binary not"
      (List [ Atom "~"; Atom "true" ])
      (UnaryExpr (BinNot, (Lexing.dummy_pos, BoolLiteral true)));
    make "integer negation"
      (List [ Atom "-"; Atom "0" ])
      (UnaryExpr (Neg, make_int_node 0));
    (* the following is pretty much testing string_of_binop function that has
       been encapsulated. *)
    make_binop "binop add" Add "+";
    make_binop "binop sub" Sub "-";
    make_binop "binop mul" Mul "*";
    make_binop "binop div" Div "/";
    make_binop "binop mod" Mod "%";
    make_binop "binop <=" Lte "<=";
    make_binop "binop >=" Gte ">=";
    make_binop "binop <" Lt "<";
    make_binop "binop >" Gt ">";
    make_binop "binop !=" Neq "!=";
    make_binop "binop ==" Deq "==";
    make_binop "binop binary OR" BinOr "|";
    make_binop "binop binary AND" BinAnd "&";
    make_binop "binop OR" Or "||";
    make_binop "binop AND" And "&&";
    make "character" (Atom "'A'") (CharLiteral 65);
    make "newline character" (Atom "'\\n'") (CharLiteral 10);
    make "classic string" (Atom "\"hello world\"")
      (StrLiteral (intq_of_lst hello_world));
    make "🐫 string" (Atom "\"\\x{01F42B}\"")
      (StrLiteral (intq_of_lst [ 128043 ]));
    make "true" (Atom "true") (BoolLiteral true);
    make "false" (Atom "false") (BoolLiteral false);
    make "empty literal" (List []) (ArrayLiteral []);
    make "singleton literal" (List [ Atom "1" ])
      (ArrayLiteral [ make_int_node 1 ]);
    make "nested literal" (List [ List [] ])
      (ArrayLiteral [ (Lexing.dummy_pos, ArrayLiteral []) ]);
    make "multiple literal"
      (List [ Atom "1"; Atom "2" ])
      (ArrayLiteral [ make_int_node 1; make_int_node 2 ]);
    make "variable" (Atom "var") (Var "var");
    make "field access"
      (List [ Atom "."; Atom "a"; Atom "x" ])
      (FieldAccess ((Lexing.dummy_pos, Var "a"), "x"));
    make "non-typed array access"
      (List [ Atom "[]"; Atom "arr"; Atom "true" ])
      (ArrayAccess
         ((Lexing.dummy_pos, Var "arr"), (Lexing.dummy_pos, BoolLiteral true)));
    make "module2 access"
      (List [ Atom "."; List [ Atom "."; Atom "A"; Atom "B" ]; Atom "x" ])
      (ModuleAccess (nodify_elements [ "A"; "B" ], "x"));
    make "module1 access"
      (List [ Atom "."; Atom "A"; Atom "x" ])
      (ModuleAccess (nodify_elements [ "A" ], "x"));
    make "int cast var"
      (List [ Atom "int"; Atom "x" ])
      (Cast ((Lexing.dummy_pos, Int 0), (Lexing.dummy_pos, Var "x")));
    make "char cast binop"
      (List [ Atom "char"; List [ Atom "+"; Atom "65"; Atom "32" ] ])
      (Cast
         ( (Lexing.dummy_pos, Char 0),
           ( Lexing.dummy_pos,
             BinopExpr (Add, make_int_node 65, make_int_node 32) ) ));
    make "function call/0"
      (List [ Atom "add0"; List [] ])
      (FunctionCall ([], "add0", []));
    make "function call /2"
      (List
         [
           Atom "add_two";
           List [ List [ Atom "+"; Atom "7"; Atom "5" ]; Atom "6" ];
         ])
      (FunctionCall
         ( [],
           "add_two",
           [
             ( Lexing.dummy_pos,
               BinopExpr (Add, make_int_node 7, make_int_node 5) );
             make_int_node 6;
           ] ));
    make "imported fn call"
      (List
         [
           Atom ".";
           List [ Atom "."; Atom "A"; Atom "B" ];
           List [ Atom "f"; List [] ];
         ])
      (FunctionCall (nodify_elements [ "A"; "B" ], "f", []));
    (* TODO: imported fn call of varying arities*)
    make "local constructor"
      (List
         [
           Atom "Queue"; List [ List [ Atom "="; Atom "max_size"; Atom "88" ] ];
         ])
      (ConstructorCall
         ([], "Queue", [ (Lexing.dummy_pos, "max_size", make_int_node 88) ]));
    make "imported constructor"
      (List
         [
           Atom ".";
           Atom "A";
           List
             [
               Atom "Color"; List [ List [ Atom "="; Atom "red"; Atom "255" ] ];
             ];
         ])
      (ConstructorCall
         ( nodify_elements [ "A" ],
           "Color",
           [ (Lexing.dummy_pos, "red", make_int_node 255) ] ));
    (* TODO: constructor A.B.Color, Imported constructor w no arguments, local
       constructor w no arguments*)
    make "null" (Atom "null") Null;
  ]

let stmt_tests =
  (* add dummy pos; the goal of this function is to literally only reduce the
     number of characters typed; rename this to somehing short if needed*)
  let anno x = (Lexing.dummy_pos, x) in
  (* short for "atom in a list"; shorthand for List [ Atom "return" ] or "break"
     or "continue"*)
  let ainl s = SexpConvert.List [ Atom s ] in
  let make name expected input_stmt =
    make_test name expected SexpConvert.sexp_of_stmt input_stmt ~printer:pp_sexp
  in
  [
    make "break" (ainl "break") Break;
    make "continue" (ainl "continue") Continue;
    make "empty block" (List []) (Block []);
    make "1 stmt block" (List [ ainl "return" ]) (Block [ anno (Return []) ]);
    make "2 stmt block"
      (List [ ainl "return"; ainl "return" ])
      (Block [ anno (Return []); anno (Return []) ]);
    make "3 stmt block"
      (List [ ainl "return"; ainl "return"; ainl "return" ])
      (Block [ anno (Return []); anno (Return []); anno (Return []) ]);
    make "return/0" (ainl "return") (Return []);
    make "return/1"
      (List [ Atom "return"; Atom "77" ])
      (Return [ make_int_node 77 ]);
    make "return/2"
      (List [ Atom "return"; Atom "77"; List [ Atom "char"; Atom "x" ] ])
      (Return
         [
           make_int_node 77;
           ( Lexing.dummy_pos,
             Cast ((Lexing.dummy_pos, Char 0), (Lexing.dummy_pos, Var "x")) );
         ]);
    make "assn2var"
      (List
         [ Atom "="; Atom "x"; List [ Atom "||"; Atom "true"; Atom "false" ] ])
      (Assign
         ( Some (Lexing.dummy_pos, Var "x"),
           anno
             (BinopExpr (Or, anno (BoolLiteral true), anno (BoolLiteral false)))
         ));
    make "assn2none"
      (List [ Atom "="; Atom "_"; List [ Atom "f"; List [] ] ])
      (Assign (None, anno (FunctionCall ([], "f", []))));
  ]

let tli_tests = []

let suite =
  "test suite for sorts" >::: List.flatten [ expr_tests; stmt_tests; tli_tests ]

let _ = run_test_tt_main suite
