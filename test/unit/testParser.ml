open Parse
open Ast
open Unit
open OUnit2

let test_buffer = Buffer.create 64

let create_generator s =
  let sedlexbuf = Sedlexing.Utf8.from_string s in
  let starting_pos, _ = Sedlexing.lexing_positions sedlexbuf in
  Sedlexing.set_position sedlexbuf { starting_pos with pos_lnum = 1 };
  Lexer.(make_token_generator (make_lexer sedlexbuf))

let parse_from_string s mode =
  let fmt = Format.formatter_of_buffer test_buffer in
  let printer = Util.SexpPrinter.make_printer fmt in
  let ast_opt =
    match parse (create_generator s) mode with
    | Ok ast ->
        Ast.SexpConvert.(print_sexp printer (sexp_of_file ast));
        Some ast
    | Error (position, msg) ->
        let l, c = Util.Position.coord_of_pos position in
        Format.fprintf fmt "%d:%d error:%s" l c msg;
        None
  in
  Format.pp_print_flush fmt ();
  let s = Buffer.contents test_buffer in
  Buffer.reset test_buffer;
  (ast_opt, s)

module PosPrinting = struct
  open Util.SexpPrinter
  open Util.Position

  (** [pp_position printer (pos, node)] ignores the value of [node] and prints
      the position. *)
  let pp_position printer (pos, _) =
    let l, c = coord_of_pos pos in
    print_atom printer (Format.sprintf "%d:%d" l c)

  let pp_list printer printf lst =
    start_list printer ();
    List.iter (printf printer) lst;
    end_list printer ()

  let pp_data_type printer node =
    start_list printer ();
    pp_position printer node;
    let _, data_type = node in
    (match data_type with
    | Int _ | Char _ | Bool _ -> ()
    | NameType (names, _, _) -> pp_list printer pp_position names);
    end_list printer ()

  let pp_name_node printer node =
    start_list printer ();
    pp_position printer node;
    end_list printer ()

  let rec pp_expr_node printer node =
    let _, expr = node in
    start_list printer ();
    pp_position printer node;
    begin
      match expr with
      | BinopExpr (_binop, _expr_node1, _expr_node2) -> ()
      | UnaryExpr (_unop, _expr_node) -> ()
      | IntLiteral _ | StrLiteral _ | CharLiteral _ | BoolLiteral _ | Null
      | Var _ ->
          ()
      | ArrayLiteral lst -> List.iter (pp_expr_node printer) lst
      | _ -> ()
    end;
    end_list printer ()

  let pp_var_decl printer (v : var_decl) =
    let _, data_type_node, name_node = v in
    start_list printer ();
    pp_data_type printer data_type_node;
    pp_name_node printer name_node;
    end_list printer ()

  let rec pp_stmt_node printer node =
    let _, stmt = node in
    start_list printer ();
    pp_position printer node;
    begin
      match stmt with
      | Break | Continue -> ()
      | Block lst -> List.iter (pp_stmt_node printer) lst
      | Return elst -> pp_list printer pp_expr_node elst
      | _ -> ()
    end;
    end_list printer ()

  let pp_module_item printer item =
    start_list printer ();
    begin
      match item with
      (* (pos(name) (pp(var_decl)* ) )*)
      | TypeDef (name_node, var_decl_list) ->
          pp_position printer name_node;
          pp_list printer pp_var_decl var_decl_list
      (* (pos(name) (pp(var_decl)* ) (pp(data_type)) (pp(stmt_node)) ) *)
      | FunctionDef ((name_node, var_decl_list, data_type_list), stmt_node_list)
        ->
          pp_position printer name_node;
          pp_list printer pp_var_decl var_decl_list;
          pp_list printer pp_data_type data_type_list;
          pp_list printer pp_stmt_node stmt_node_list
      (* (central pos (type pos) (id pos) (?expr pos)) *)
      | GlobalVarDecl (pos, var_decl, expr_node_opt) -> (
          pp_position printer (pos, ());
          pp_var_decl printer var_decl;
          match expr_node_opt with
          | None ->
              start_list printer ();
              end_list printer ()
          | Some expr_node -> pp_expr_node printer expr_node)
    end;
    end_list printer ()

  let pp_module_items printer items =
    start_list printer ();
    List.iter (pp_module_item printer) items;
    end_list printer ()
end

let str_of_node_positions =
  let buffer = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buffer in
  let printer = Util.SexpPrinter.make_printer_custom fmt 0 in
  let open PosPrinting in
  function
  | Module (imports, module_nodes) ->
      Util.SexpPrinter.start_list printer ();
      pp_list printer pp_name_node imports;
      pp_module_items printer module_nodes;
      Util.SexpPrinter.end_list printer ();
      Format.pp_print_flush fmt ();
      let str = Buffer.contents buffer in
      Buffer.clear buffer;
      str
  | Interface _ -> failwith "todo: test interface"

let combo name ~ast ~pos ~input mode =
  let module_node, printed_ast = parse_from_string input mode in
  let positions =
    match module_node with
    | None -> "(error)"
    | Some node -> str_of_node_positions node
  in
  (* just take the string value. Nothing to do with input aside from direct
     comparison with expected output. *)
  let id s = s in
  [
    make_test (name ^ " AST") ast id printed_ast ~printer:pp_string
      ~cmp:Eth.equal_sexp_str;
    make_test name pos id positions ~printer:pp_string ~cmp:Eth.equal_sexp_str;
  ]

(** [combo_m name ~ast ~pos ~input] runs 2 tests on [input] where the first
    parses [input] as a module and compares against [ast]. The second test
    computes the important positions stored in the AST and compares against
    [pos]. *)
let combo_m name ~ast ~pos ~input = combo name ~ast ~pos ~input `Module

(** [combo_i] is exactly like [combo_m] but parses the given input using
    interface grammar.*)
let combo_i name ~ast ~pos ~input = combo name ~ast ~pos ~input `Interface

let combo_tli_tests =
  [
    combo_m "empty module" ~ast:"(() ())" ~pos:"(() ())" ~input:"";
    combo_m "imports" ~ast:"(((import M) (import T))())"
      ~input:"import M import T" ~pos:"(((1:8) (1:17))())";
    combo_m "global var default" ~ast:"(() ((:global x int)))"
      ~pos:"(() ((1:5 ((1:1) (1:5)) ())))" ~input:"int x;";
    combo_m "global var init" ~ast:"(() ((:global x int true)))"
      ~pos:"(()( (1:7 ((1:1) (1:5)) (1:9)) ))" ~input:"int x = true;";
    combo_m "procedure definition" ~ast:"(() ( (main () () ((return))) ))"
      ~pos:"(() ((1:6 () () ((1:14 () )) )) )" ~input:"void main(){ return; }";
    combo_m "single return function" ~ast:"(() ((main () (int) ((return 1)))))"
      ~pos:"(() ((1:5 () ((1:1)) ((2:1 ((2:8)))) )) )"
      ~input:"int main(){\nreturn 1;}";
    combo_m "return-2 function" ~ast:"(()( (pair () (int int) ()) ))"
      ~pos:"(() ((1:9 () ((1:1) (1:5)) ()) ))" ~input:"int,int pair(){}";
    combo_m "return-many function" ~ast:"(() ((f () (int bool char List) () )))"
      ~input:"int,\nbool,\nchar,\nList\nf(){}"
      ~pos:"(()( (5:1 () ((1:1) (2:1) (3:1) (4:1 ())) ()) ))";
    combo_m "return array func" ~ast:"(()((main () (([] ([] int))) ())))"
      ~pos:"(() ( (1:9 () ((1:1)) ()) ) )" ~input:"int[][] main(){}";
    combo_m "type def 0 field" ~ast:"(()((X ())))" ~pos:"(()((1:6 ())))"
      ~input:"type X = {}";
    combo_m "type def 1 field" ~ast:"(()((Y ((x int)))))"
      ~pos:"(()((1:6 (((1:12) (1:16))))))" ~input:"type Y = { int x; }";
    combo_m "type def many field"
      ~ast:"(()((Z ((x int) (y bool) (z char) (s String)))))"
      ~pos:"(()((1:6 (((2:1)(2:5))((3:1)(3:6))((4:1)(4:6))((5:1 ())(5:8))))))"
      ~input:"type Z = {\nint x;\nbool y;\nchar z;\nString s;\n}";
  ]

let stmt_tests = []

let error_test =
  let error_m name ~ast ~input = combo_m name ~ast ~input ~pos:"(error)" in
  (* for convenience, easily modifiable to reflect error message updates. *)
  let not_a_loc = "a value is not a variable/location" in
  let not_fun_call = "a function call is expected" in
  let not_a_statement = "not a statement" in
  [
    error_m "value as statement"
      ~ast:("2:1 error:" ^ not_a_statement)
      ~input:"main(){\na;}";
    error_m "value as location" ~input:"main(){\n1=1;}"
      ~ast:("2:1 error:" ^ not_a_loc);
    error_m "value as location in multi-assign" ~input:"main(){\nx,1=1;}"
      ~ast:("2:3 error:" ^ not_a_loc);
    error_m "invalid usage of multi-assign" ~input:"f(){\nx,y=3;}"
      ~ast:("2:5 error:" ^ not_fun_call);
    error_m "unexpected LCBRAC" ~ast:"1:1 error:unexpected '{'" ~input:"{}";
    error_m "unexpected RCBRAC" ~ast:"1:5 error:unexpected '}'" ~input:"int }";
    error_m "double while token" ~ast:"2:7 error:unexpected while"
      ~input:"char error(){\nwhile while}";
    error_m "unexpected void: invalid ret type" ~ast:"1:5 error:unexpected void"
      ~input:"int,void lol()";
    error_m "unexpected minus" ~ast:"1:1 error:unexpected '-'" ~input:"- f(){}";
    error_m "empty statement" ~ast:"2:1 error:unexpected ';'"
      ~input:"main(){\n;}";
    error_m "unexpected add" ~ast:"2:1 error:unexpected '+'" ~input:"\n+";
    error_m "unexpected mult" ~ast:"1:1 error:unexpected '*'" ~input:"****";
    error_m "unexpected string" ~ast:"2:1 error:unexpected string"
      ~input:"type T =\n\"some fields\"";
    error_m "unexpected integer" ~ast:"2:1 error:unexpected integer"
      ~input:"import\n1000";
    error_m "unexpected uppercase identifier"
      ~ast:"1:8 error:unexpected type identifier" ~input:"type X Y = {}";
    error_m "unexpected identifier" ~ast:"1:7 error:unexpected identifier"
      ~input:"int x y = 3";
    error_m "unexpected character" ~input:"\n'a''b''c'"
      ~ast:"2:1 error:unexpected character";
    error_m "invalid global usage of wild card"
      ~ast:"1:1 error:unexpected underscore" ~input:"_ = 4;";
    error_m "unexpected (" ~input:"T()" ~ast:"1:2 error:unexpected '('";
    error_m "unexpected )" ~input:"T)" ~ast:"1:2 error:unexpected ')'";
    error_m "unexpected eof" ~input:"T[]" ~ast:"1:4 error:unexpected EOF";
    error_m "unexpected nested type" ~input:"type T = {\ntype X;}"
      ~ast:"2:1 error:unexpected keyword type";
  ]

let suite =
  "test suite for parser"
  >::: List.flatten (combo_tli_tests @ stmt_tests @ error_test)

let _ = run_test_tt_main suite