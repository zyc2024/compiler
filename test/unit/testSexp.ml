(** This test script verifies the conversion from AST syntax trees to
    S-expressions. *)

open OUnit2
open Ast
open Unit

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

  let nodify_elements l =
    List.rev (List.rev_map (fun x -> (Lexing.dummy_pos, x)) l)
end

open Helper

let expr_tests =
  (* [make name expected input] runs an Ounit test by converting the input to an
     sexp and comparing it against the given sexp [expected]. *)
  let make name expected input_expr =
    make_test name expected SexpConvert.sexp_of_expr input_expr ~printer:pp_sexp
      ~cmp:( = )
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
      ~cmp:( = )
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
    make "multiassn"
      (List
         [
           Atom "=";
           List
             [
               List
                 [
                   Atom ".";
                   List
                     [
                       Atom "."; List [ Atom "."; Atom "A"; Atom "B" ]; Atom "C";
                     ];
                   Atom "aModVar";
                 ];
               Atom "x";
             ];
           List [ Atom "returnPair"; List [ Atom "77" ] ];
         ])
      (MultiAssign
         ( [
             Some
               (anno
                  (ModuleAccess (nodify_elements [ "A"; "B"; "C" ], "aModVar")));
             Some (anno (Var "x"));
           ],
           anno (FunctionCall ([], "returnPair", [ make_int_node 77 ])) ));
    make "multiassn w blank"
      (List
         [
           Atom "=";
           List [ Atom "_"; Atom "x"; Atom "_" ];
           List [ Atom "returnTriplet"; List [] ];
         ])
      (MultiAssign
         ( [ None; Some (Lexing.dummy_pos, Var "x"); None ],
           (Lexing.dummy_pos, FunctionCall ([], "returnTriplet", [])) ));
    (* TODO: declaration, multideclaration, arrayInit*)
    make "decl bool"
      (List [ Atom "x"; Atom "bool" ])
      (Declaration ((false, anno (Bool 0), anno "x"), None));
    make "decl final name"
      (List [ Atom "x"; Atom "const"; Atom "Color" ])
      (Declaration ((true, anno (NameType ([], "Color", 0)), anno "x"), None));
    make "decl name of dim 5"
      (List
         [
           Atom "x";
           List
             [
               Atom "[]";
               List
                 [
                   Atom "[]";
                   List
                     [
                       Atom "[]";
                       List [ Atom "[]"; List [ Atom "[]"; Atom "Color" ] ];
                     ];
                 ];
             ];
         ])
      (Declaration ((false, anno (NameType ([], "Color", 5)), anno "x"), None));
    make "decl w assn"
      (List [ Atom "="; List [ Atom "x"; Atom "bool" ]; Atom "true" ])
      (Declaration
         ((false, anno (Bool 0), anno "x"), Some (anno (BoolLiteral true))));
    make "multidecl"
      (List [ Atom "x"; Atom "y"; Atom "z"; Atom "int" ])
      (MultiDeclaration (false, anno (Int 0), nodify_elements [ "x"; "y"; "z" ]));
    make "multidecl const arrs"
      (List
         [
           Atom "x";
           Atom "y";
           Atom "z";
           Atom "const";
           List [ Atom "[]"; List [ Atom "[]"; Atom "int" ] ];
         ])
      (MultiDeclaration (true, anno (Int 2), nodify_elements [ "x"; "y"; "z" ]));
    make "array init one dim"
      (List [ Atom "="; Atom "x"; List [ Atom "[]"; Atom "int"; Atom "77" ] ])
      (ArrayInit (anno (Int 1), [ make_int_node 77 ], "x"));
    make "array partial init"
      (List
         [
           Atom "=";
           Atom "x";
           List
             [
               Atom "[]";
               List [ Atom "[]"; List [ Atom "[]"; Atom "int" ]; Atom "77" ];
               Atom "55";
             ];
         ])
      (ArrayInit (anno (Int 3), [ make_int_node 55; make_int_node 77 ], "x"));
    make "array init char"
      (List
         [
           Atom "=";
           Atom "x";
           List
             [
               Atom "[]";
               List [ Atom "[]"; List [ Atom "[]"; Atom "char" ] ];
               Atom "6";
             ];
         ])
      (ArrayInit (anno (Char 3), [ anno (IntLiteral 6L) ], "x"));
    make "array init bool"
      (List [ Atom "="; Atom "x"; List [ Atom "[]"; Atom "bool"; Atom "6" ] ])
      (ArrayInit
         ((Lexing.dummy_pos, Bool 1), [ (Lexing.dummy_pos, IntLiteral 6L) ], "x"));
    make "array init colors"
      (List
         [
           Atom "=";
           Atom "colors";
           List
             [
               Atom "[]";
               List [ Atom "."; Atom "Colors"; Atom "Color" ];
               List [ Atom "+"; Atom "1"; Atom "2" ];
             ];
         ])
      (ArrayInit
         ( anno (NameType ([ anno "Colors" ], "Color", 1)),
           [
             anno (BinopExpr (Add, anno (IntLiteral 1L), anno (IntLiteral 2L)));
           ],
           "colors" ));
    make "call local procedure"
      (List
         [
           Atom "doSomething";
           List [ Atom "true"; Atom "x"; List [ Atom "."; Atom "A"; Atom "x" ] ];
         ])
      (ProcedureCall
         ( [],
           "doSomething",
           [
             anno (BoolLiteral true);
             anno (Var "x");
             anno (ModuleAccess ([ anno "A" ], "x"));
           ] ));
    make "call foreign procedure"
      (List
         [
           Atom ".";
           List [ Atom "."; Atom "A"; Atom "B" ];
           List [ Atom "doAnotherThing"; List [ Atom "x"; Atom "y" ] ];
         ])
      (ProcedureCall
         ( nodify_elements [ "A"; "B" ],
           "doAnotherThing",
           nodify_elements [ Var "x"; Var "y" ] ));
    make "if stmt"
      (List
         [
           Atom "if";
           List [ Atom "<"; Atom "x"; Atom "y" ];
           List [ ainl "return"; ainl "break" ];
           List [];
         ])
      (If
         ( anno (BinopExpr (Lt, anno (Var "x"), anno (Var "y"))),
           anno (Block [ anno (Return []); anno Break ]),
           None ));
    make "if-else stmt"
      (List
         [
           Atom "if";
           List [ Atom "!"; Atom "false" ];
           ainl "continue";
           ainl "break";
         ])
      (If
         ( anno (UnaryExpr (Not, anno (BoolLiteral false))),
           anno Continue,
           Some (anno Break) ));
    make "while loop"
      (List [ Atom "while"; Atom "true"; ainl "continue" ])
      (While (anno (BoolLiteral true), anno Continue));
    (* TODO: maybe test every combination? 2*2*2 = 8; seems largely unnecessary
       though*)
    (* TODO: for7, for3; for5; for 6*)
    make "for7"
      (List
         [
           Atom "for";
           List
             [
               Atom "=";
               List [ Atom "x"; List [ Atom "."; Atom "Colors"; Atom "Color" ] ];
               Atom "color_green";
             ];
           List
             [
               Atom ".";
               Atom "Colors";
               List [ Atom "isBlue"; List [ Atom "x" ] ];
             ];
           List
             [
               Atom "=";
               Atom "x";
               List
                 [
                   Atom ".";
                   Atom "Colors";
                   List [ Atom "nextColor"; List [ Atom "x" ] ];
                 ];
             ];
           ainl "break";
         ])
      (For
         ( Some
             (anno
                (Declaration
                   ( ( false,
                       anno
                         (NameType (nodify_elements [ "Colors" ], "Color", 0)),
                       anno "x" ),
                     Some (anno (Var "color_green")) ))),
           Some
             (anno
                (FunctionCall ([ anno "Colors" ], "isBlue", [ anno (Var "x") ]))),
           Some
             (anno
                (Assign
                   ( Some (anno (Var "x")),
                     anno
                       (FunctionCall
                          ([ anno "Colors" ], "nextColor", [ anno (Var "x") ]))
                   ))),
           anno Break ));
    make "for0"
      (List [ Atom "for"; List []; List []; List []; ainl "break" ])
      (For (None, None, None, anno Break));
  ]

let file_tests =
  let sfile_wrapper x = SexpConvert.List [ List []; List [ x ] ] in
  let astfile_wrapper x = Module ([], [ x ]) in

  let make n expect input =
    make_test n expect SexpConvert.sexp_of_file input ~printer:pp_sexp
      ~cmp:( = )
  in
  let write_import n = SexpConvert.List [ Atom "import"; Atom n ] in
  [
    make "empty" (List [ List []; List [] ]) (Module ([], []));
    make "1 import"
      (List [ List [ write_import "A" ]; List [] ])
      (Module (nodify_elements [ "A" ], []));
    make "2 import"
      (List [ List [ write_import "A"; write_import "B" ]; List [] ])
      (Module (nodify_elements [ "A"; "B" ], []));
    make "global int"
      (sfile_wrapper (List [ Atom ":global"; Atom "x"; Atom "int" ]))
      (astfile_wrapper
         (GlobalVarDecl
            ( Lexing.dummy_pos,
              (false, (Lexing.dummy_pos, Int 0), (Lexing.dummy_pos, "x")),
              None )));
    make "global bool w init"
      (sfile_wrapper
         (List [ Atom ":global"; Atom "x"; Atom "bool"; Atom "true" ]))
      (astfile_wrapper
         (GlobalVarDecl
            ( Lexing.dummy_pos,
              (false, (Lexing.dummy_pos, Bool 0), (Lexing.dummy_pos, "x")),
              Some (Lexing.dummy_pos, BoolLiteral true) )));
    make "global const"
      (sfile_wrapper
         (List
            [ Atom ":global"; Atom "x"; Atom "const"; Atom "bool"; Atom "true" ]))
      (astfile_wrapper
         (GlobalVarDecl
            ( Lexing.dummy_pos,
              (true, (Lexing.dummy_pos, Bool 0), (Lexing.dummy_pos, "x")),
              Some (Lexing.dummy_pos, BoolLiteral true) )));
    make "emptytype"
      (sfile_wrapper (List [ Atom "EType"; List [] ]))
      (astfile_wrapper (TypeDef ((Lexing.dummy_pos, "EType"), [])));
    make "color type def"
      (sfile_wrapper
         (List
            [
              Atom "Color";
              List
                [
                  List [ Atom "red"; Atom "const"; Atom "ColorPoint" ];
                  List [ Atom "green"; Atom "const"; Atom "ColorPoint" ];
                  List [ Atom "blue"; Atom "const"; Atom "ColorPoint" ];
                ];
            ]))
      (astfile_wrapper
         (TypeDef
            ( (Lexing.dummy_pos, "Color"),
              [
                ( true,
                  (Lexing.dummy_pos, NameType ([], "ColorPoint", 0)),
                  (Lexing.dummy_pos, "red") );
                ( true,
                  (Lexing.dummy_pos, NameType ([], "ColorPoint", 0)),
                  (Lexing.dummy_pos, "green") );
                ( true,
                  (Lexing.dummy_pos, NameType ([], "ColorPoint", 0)),
                  (Lexing.dummy_pos, "blue") );
              ] )));
    make "0->1 fndef"
      (sfile_wrapper
         (List
            [
              Atom "zero";
              List [];
              List [ Atom "int" ];
              List [ List [ Atom "return"; Atom "0" ] ];
            ]))
      (astfile_wrapper
         (FunctionDef
            ( ((Lexing.dummy_pos, "zero"), [], [ (Lexing.dummy_pos, Int 0) ]),
              [
                (Lexing.dummy_pos, Return [ (Lexing.dummy_pos, IntLiteral 0L) ]);
              ] )));
    make "2->2 fndef"
      (sfile_wrapper
         (List
            [
              Atom "idempair";
              List
                [ List [ Atom "x"; Atom "int" ]; List [ Atom "y"; Atom "int" ] ];
              List [ Atom "int"; Atom "int" ];
              List [ List [ Atom "return"; Atom "x"; Atom "y" ] ];
            ]))
      (astfile_wrapper
         (FunctionDef
            ( ( (Lexing.dummy_pos, "idempair"),
                [
                  (false, (Lexing.dummy_pos, Int 0), (Lexing.dummy_pos, "x"));
                  (false, (Lexing.dummy_pos, Int 0), (Lexing.dummy_pos, "y"));
                ],
                [ (Lexing.dummy_pos, Int 0); (Lexing.dummy_pos, Int 0) ] ),
              [
                ( Lexing.dummy_pos,
                  Return
                    [ (Lexing.dummy_pos, Var "x"); (Lexing.dummy_pos, Var "y") ]
                );
              ] )));
  ]

let large = 10000

let expr_large_tests =
  let make name expected input_expr =
    make_test
      (Format.sprintf "%s of size %d" name large)
      expected SexpConvert.sexp_of_expr input_expr ~printer:pp_sexp ~cmp:( = )
  in
  let make_expr_lst size =
    let exprs =
      List.rev_map (fun n -> IntLiteral (Int64.of_int n)) (1 -<- size)
    in
    nodify_elements exprs
  in
  let make_arrlit_sexp size =
    SexpConvert.List
      (List.rev_map (fun n -> SexpConvert.Atom (string_of_int n)) (1 -<- size))
  in
  [
    make "array literal" (make_arrlit_sexp large)
      (ArrayLiteral (make_expr_lst large));
  ]

let suite =
  "test suite for s-expressions"
  >::: List.flatten [ expr_tests; stmt_tests; file_tests; expr_large_tests ]

let _ = run_test_tt_main suite
