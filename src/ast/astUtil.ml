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

(* let rec print_expr expr out_chan = match expr with | BinopExpr (o, (_, e1),
   (_, e2)) -> Printf.fprintf out_chan "(%s " (binop_to_string o); print_expr e1
   out_chan; print_expr e2 out_chan; Printf.fprintf out_chan ")" | UnaryExpr (o,
   (_, e)) -> Printf.fprintf out_chan "(%s " (unop_to_string o); print_expr e
   out_chan; Printf.fprintf out_chan ")" | IntLiteral v -> Int64.to_string v |>
   print_endline | StrLiteral iq -> Printf.fprintf out_chan "{"; Queue.iter (fun
   x -> Printf.fprintf out_chan "%d" x) iq; Printf.fprintf out_chan "}" |
   CharLiteral v -> Printf.fprintf out_chan "%d" v | BoolLiteral b ->
   Printf.fprintf out_chan "%s" (if b then "true" else "false") | ArrayLiteral _
   -> (* List.iter (fun x = match x with (_, e) -> print_expr e | _ ->
   print_endline "failed") a *) failwith "array literal" | _ -> exit 2 *)

open Core.Sexp

(* alias for readability *)
type sexp = t

let rec sexp_of_expr = function
  | BinopExpr (op, (_, e1), (_, e2)) ->
      List [ Atom (binop_to_string op); sexp_of_expr e1; sexp_of_expr e2 ]
  | UnaryExpr (op, (_, e)) -> List [ Atom (unop_to_string op); sexp_of_expr e ]
  | IntLiteral long -> Atom (Int64.to_string long)
  | CharLiteral integer -> Atom (Int.to_string integer)
  | BoolLiteral b -> Atom (Bool.to_string b)
  | ArrayLiteral enode_list ->
      (* all lists are stored in reversed order so this looping process
         naturally puts the expressions in order *)
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
