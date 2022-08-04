open AstNode

let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lte -> "<="
  | Gte -> ">="
  | Lt -> "<"
  | Gt -> ">"
  | Neq -> "!="
  | Deq -> "=="
  | BinOr -> "|"
  | BinAnd -> "&"
  | Or -> "||"
  | And -> "&&"

let unop_to_string = function Neg -> "-" | Not -> "!" | BinNot -> "~"

open Core.Sexp

let rec sexp_of_expr = function
  | BinopExpr (op, (_, e1), (_, e2)) ->
      List [ Atom (binop_to_string op); sexp_of_expr e1; sexp_of_expr e2 ]
  | UnaryExpr (op, (_, e)) -> List [ Atom (unop_to_string op); sexp_of_expr e ]
  | IntLiteral long -> Atom (Int64.to_string long)
  | CharLiteral integer -> Atom (Int.to_string integer)
  | BoolLiteral b -> Atom (Bool.to_string b)
  | ArrayLiteral enode_list ->
      let rec loop acc lst =
        match lst with
        | [] -> acc
        | (_, e) :: t -> loop (sexp_of_expr e :: acc) t
      in
      List (loop [] enode_list)
  | Cast (_, _) -> failwith "todo cast sexp"
  | FunctionCall (_, name, _) -> List [ Atom name; List [] ]
  | Var var_name -> Atom var_name
  | FieldAccess ((_, e), field_name) ->
      List [ Atom "."; sexp_of_expr e; Atom field_name ]
  | ArrayAccess ((_, ptr), (_, index)) ->
      List [ Atom "[]"; sexp_of_expr ptr; sexp_of_expr index ]
  | ModuleAccess _ -> failwith "todo module access sexp"
  | _ -> failwith "TODO sexp_of_expr"

let rec sexp_of_stmt = function
  | Break -> List [ Atom "break" ]
  | Continue -> List [ Atom "continue" ]
  | Block stmt_node_list ->
      List
        (List.fold_left
           (fun acc (_, stmt) -> sexp_of_stmt stmt :: acc)
           [] stmt_node_list)
  | Assign (dest, (_, src_expr)) ->
      List
        [
          Atom "=";
          (match dest with Some (_, e) -> sexp_of_expr e | None -> Atom "_");
          sexp_of_expr src_expr;
        ]
  | MultiAssign (dest_list, (_, src_expr)) -> begin
      let dest_list_sexp =
        List.fold_left
          (fun acc enode ->
            match enode with
            | Some (_, e) -> sexp_of_expr e :: acc
            | None -> Atom "_" :: acc)
          [] dest_list
      in
      List [ Atom "="; List dest_list_sexp; sexp_of_expr src_expr ]
    end
  | If ((_, cond_expr), (_, s), else_branch) -> (
      let if_sexp = [ Atom "if"; sexp_of_expr cond_expr; sexp_of_stmt s ] in
      match else_branch with
      | None -> List if_sexp
      | Some (_, s2) -> List (if_sexp @ [ sexp_of_stmt s2 ]))
  | _ -> failwith "TODO sexp_of_stmt"

let sexp_of_import import =
  match import with _, id -> List [ Atom "import"; Atom id ]

let sexp_of_global = function
  | TypeDef (_, _) -> failwith "85 astUtil"
  | FunctionDef (function_decl, stmt_node_list) ->
      let name, _, _ = function_decl in
      (* printing the function body (stmt^{*}) *)
      List
        [
          Atom name;
          List
            (List.map (function _, stmt -> sexp_of_stmt stmt) stmt_node_list);
        ]
  | GlobalVarDecl _ -> failwith ""

let sexp_of_file = function
  | Interface -> failwith "todo sexp interface"
  | Module (imports, global_items) ->
      let import_sexp =
        List (List.map (fun import -> sexp_of_import import) imports)
      in
      let globals_sexp =
        List (List.map (fun (_, global) -> sexp_of_global global) global_items)
      in
      List [ import_sexp; globals_sexp ]

open Util

let rec print_sexp fmt = function
  | Atom s -> SexpPrinter.print_atom fmt s
  | List sexp_list ->
      SexpPrinter.(
        start_list fmt ();
        List.iter (fun sexp -> print_sexp fmt sexp) sexp_list;
        end_list fmt ())

(* ===================== NEW ============================================ *)

(* open Util.SexpPrinter

   let print_as_list fmt print_content = start_list fmt (); print_content ();
   end_list fmt ()

   let rec print_expr fmt = function | BinopExpr (op, (_, e1), (_, e2)) ->
   start_list fmt (); print_atom (binop_to_string op); print_expr e1; print_expr
   e2; end_list () | UnaryExpr (op, (_, e)) -> start_list (); print_atom
   (unop_to_string op); print_expr e; end_list () | IntLiteral long ->
   print_atom (Int64.to_string long) | CharLiteral integer -> print_atom
   (Int.to_string integer) | BoolLiteral b -> print_atom (Bool.to_string b) |
   ArrayLiteral enode_list -> start_list (); List.iter (fun (_, e) -> print_expr
   e) enode_list; end_list () | FunctionCall (_, name, arg_node_list) ->
   start_list (); print_atom name; start_list (); List.iter (fun (_, e) ->
   print_expr e) arg_node_list; end_list (); end_list () | Var var_name ->
   print_atom var_name | _ -> failwith "todo"

   let rec print_stmt = function | Break -> (fun () -> print_atom "break") |>
   print_as_list | Block stmt_node_list -> print_as_list (fun () -> List.iter
   (fun (_, stmt) -> print_stmt stmt) (List.rev stmt_node_list)) | Assign (dest,
   (_, src_expr)) -> print_as_list (fun () -> print_atom "="; (match dest with |
   Some (_, e) -> print_expr e | None -> print_atom "_"); print_expr src_expr) |
   MultiAssign (dest_list, (_, src_expr)) -> let print_dest_list () = List.iter
   (function Some (_, e) -> print_expr e | None -> print_atom "_") (List.rev
   dest_list) in print_as_list (fun () -> print_atom "="; print_as_list
   print_dest_list; print_expr src_expr) | If ((_, cond_expr), (_, s),
   else_branch) -> ( let print_if_branch () = print_atom "if"; print_expr
   cond_expr; print_stmt s in match else_branch with | None -> print_as_list
   print_if_branch | Some (_, s2) -> print_as_list (fun () -> print_if_branch
   (); print_stmt s2)) | For (stmt1_opt, expr_opt, stmt2_opt, (_, stmt)) ->
   start_list (); print_atom "for"; let print_optional_stmt = function | None ->
   start_list (); end_list () | Some (_, s) -> print_stmt s in
   print_optional_stmt stmt1_opt; (match expr_opt with | None -> start_list ();
   end_list () | Some (_, e) -> print_expr e); print_optional_stmt stmt2_opt;
   print_stmt stmt; end_list () | _ -> failwith "TODO astUtil print_stmt" *)
