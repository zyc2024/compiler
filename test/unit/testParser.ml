open Parse
open Unit
open OUnit2
open Ast

let test_buffer = Buffer.create 64

let create_generator s =
  let sedlexbuf = Sedlexing.Utf8.from_string s in
  let starting_pos, _ = Sedlexing.lexing_positions sedlexbuf in
  Sedlexing.set_position sedlexbuf { starting_pos with pos_lnum = 1 };
  let tokenizer _ = Lex.Lexer.(tokenize (make_lexer sedlexbuf)) in
  Sedlexing.with_tokenizer tokenizer sedlexbuf

let parse_from_string s mode =
  let fmt = Format.formatter_of_buffer test_buffer in
  let ast_opt =
    match parse_with_output (create_generator s) mode fmt with
    | Ok ast -> Some ast
    | Error _ -> None
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
    pp_data_type printer data_type_node;
    pp_name_node printer name_node

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
      | TypeDef (_, _var_decl_node_list) -> ()
      (* (pos(name) (pp(var_decl)* ) (pp(data_type)) ) *)
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
      pp_list printer pp_position imports;
      pp_module_items printer module_nodes;
      Util.SexpPrinter.end_list printer ();
      Format.pp_print_flush fmt ();
      let str = Buffer.contents buffer in
      Buffer.clear buffer;
      str
  | Interface -> ""

let combo_m name ~ast ~pos ~input =
  let module_node, printed_ast = parse_from_string input `Module in
  let positions =
    match module_node with
    | None -> "(error)"
    | Some node -> str_of_node_positions node
  in
  let id s = s in
  [
    make_test (name ^ " AST") ast id printed_ast ~printer:pp_string
      ~cmp:Eth.equal_sexp_str;
    make_test name pos id positions ~printer:pp_string ~cmp:Eth.equal_sexp_str;
  ]

let combo_tests =
  [
    combo_m "empty module" ~ast:"(() ())" ~pos:"(() ())" ~input:"";
    combo_m "global var default" ~ast:"(() ((:global x int)))"
      ~pos:"(() ((1:5 (1:1) (1:5) ())))" ~input:"int x;";
    combo_m "global var init" ~ast:"(() ((:global x int true)))"
      ~pos:"(()( (1:7 (1:1) (1:5) (1:9)) ))" ~input:"int x = true;";
    combo_m "procedure definition" ~ast:"(() ( (main () () ((return))) ))"
      ~pos:"(() ((1:6 () () ((1:14 () )) )) )" ~input:"void main(){ return; }";
    combo_m "single return function" ~ast:"(() ((main () (int) ((return 1)))))"
      ~pos:"(() ((1:5 () ((1:1)) ((2:1 ((2:8)))) )) )"
      ~input:"int main(){\nreturn 1;}";
    combo_m "multi return function" ~ast:"(()( (pair () (int int) ()) ))"
      ~pos:"(() ((1:9 () ((1:1) (1:5)) ()) ))" ~input:"int,int pair(){}";
  ]

let suite = "test suite for parser" >::: List.flatten combo_tests
let _ = run_test_tt_main suite