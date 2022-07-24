open AstNode

(* use a BNF style ?*)
(* u wanna try Xi style BNF? idk to_string seems expensive if the goal is to
   print. look at usage of jane street sexp?*)

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

let rec print_expr expr out_chan =
  match expr with
  | BinopExpr (o, (_, e1), (_, e2)) ->
      Printf.fprintf out_chan "(%s " (binop_to_string o);
      print_expr e1 out_chan;
      print_expr e2 out_chan;
      Printf.fprintf out_chan ")"
  | UnaryExpr (o, (_, e)) ->
      Printf.fprintf out_chan "(%s " (unop_to_string o);
      print_expr e out_chan;
      Printf.fprintf out_chan ")"
  | IntLiteral v -> Int64.to_string v |> print_endline
  | StrLiteral iq ->
      Printf.fprintf out_chan "{";
      Queue.iter (fun x -> Printf.fprintf out_chan "%d" x) iq;
      Printf.fprintf out_chan "}"
  | CharLiteral v -> Printf.fprintf out_chan "%d" v
  | BoolLiteral b ->
      Printf.fprintf out_chan "%s" (if b then "true" else "false")
  | ArrayLiteral _ ->
      (* List.iter (fun x = match x with (_, e) -> print_expr e | _ ->
         print_endline "failed") a *)
      failwith "array literal"
  | LhsExpr (_, de) -> begin
      match de with
      | Var s -> Printf.fprintf out_chan "%s" s
      | FieldAccess ((_, e), s) ->
          print_expr e out_chan;
          Printf.fprintf out_chan "%s" s
      | _ -> exit 1
    end
  | _ -> exit 2
