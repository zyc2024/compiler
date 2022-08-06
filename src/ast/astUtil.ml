open AstNode

type sexp =
  | Atom of string
  | List of sexp list

let blank_lhs = Atom "_"

let sexp_of_binop = function
  | Add -> Atom "+"
  | Sub -> Atom "-"
  | Mul -> Atom "*"
  | Div -> Atom "/"
  | Mod -> Atom "%"
  | Lte -> Atom "<="
  | Gte -> Atom ">="
  | Lt -> Atom "<"
  | Gt -> Atom ">"
  | Neq -> Atom "!="
  | Deq -> Atom "=="
  | BinOr -> Atom "|"
  | BinAnd -> Atom "&"
  | Or -> Atom "||"
  | And -> Atom "&&"

let sexp_of_unop = function
  | Neg -> Atom "-"
  | Not -> Atom "!"
  | BinNot -> Atom "~"

let rec sexp_of_srclist k =
  match k with
  | [ (_, s) ] -> Atom s
  | [ (_, s1); (_, s2) ] -> List [ Atom "."; Atom s1; Atom s2 ]
  | (_, s) :: t -> List [ Atom "."; sexp_of_srclist t; Atom s ]
  | [] -> Atom ""

let n_lrsbrac n =
  let rec aux k acc = if k = 0 then acc else aux (k - 1) acc ^ "[]" in
  aux n ""

let sexp_of_datatype dt =
  let rec tformat t d =
    match d with
    | 0 -> t
    (* | 1 -> List [ Atom "[]"; t ] *)
    | _ -> tformat (List [ Atom "[]"; t ]) (d - 1)
  in
  match dt with
  | Int i -> tformat (Atom "int") i
  | Char i -> tformat (Atom "char") i
  | Bool i -> tformat (Atom "bool") i
  | NameType (slist, n, i) ->
      let k =
        match slist with
        | _ :: _ -> List [ Atom "."; sexp_of_srclist slist; Atom n ]
        | _ -> Atom n
      in
      tformat k i

let rec sexp_of_expr = function
  | BinopExpr (o, (_, e1), (_, e2)) ->
      List [ sexp_of_binop o; sexp_of_expr e1; sexp_of_expr e2 ]
  | UnaryExpr (o, (_, e)) -> List [ sexp_of_unop o; sexp_of_expr e ]
  | IntLiteral i -> Atom (Int64.to_string i)
  | StrLiteral s ->
      Atom (Printf.sprintf "\"%s\"" (Util.Unicode.string_of_intq s))
  | CharLiteral c -> Atom (Int.to_string c)
  | BoolLiteral b -> Atom (if b then "true" else "false")
  | ArrayLiteral l ->
      List
        (let rec pelist elist =
           match elist with
           | (_, e) :: t -> sexp_of_expr e :: pelist t
           | [] -> []
         in
         pelist l)
  | Var s -> Atom s
  | FieldAccess ((_, e), s) -> List [ Atom "."; sexp_of_expr e; Atom s ]
  | ArrayAccess ((_, e1), (_, e2)) ->
      List [ Atom "[]"; sexp_of_expr e1; sexp_of_expr e2 ]
  | ModuleAccess (slist, s) ->
      List [ Atom "moduleAccess"; sexp_of_srclist slist; Atom s ]
  | Cast ((_, dt), (_, e)) ->
      List [ Atom "cast"; sexp_of_datatype dt; sexp_of_expr e ]
  | FunctionCall (slist, n, alist) -> (
      match slist with
      | _ :: _ ->
          List
            [
              Atom ".";
              sexp_of_srclist slist;
              List (Atom n :: sexps_of_expr_list alist);
            ]
      | _ -> List (Atom "()" :: Atom n :: sexps_of_expr_list alist))
  | ConstructorCall (slist, n, named_args) -> (
      match slist with
      | _ :: _ ->
          List
            [
              Atom ".";
              sexp_of_srclist slist;
              List (Atom n :: named_args_to_sexp named_args);
            ]
      | _ -> List (Atom "()" :: Atom n :: named_args_to_sexp named_args))
  | Null -> Atom "null"

and sexps_of_expr_list elist =
  List.(rev (rev_map (fun (_, e) -> sexp_of_expr e) elist))

and named_args_to_sexp elist =
  let rec aux l =
    match l with
    | (_, n, (_, e)) :: t -> List [ Atom "="; Atom n; sexp_of_expr e ] :: aux t
    | [] -> []
  in
  aux elist

let rec sexp_of_stmt = function
  | Break -> List [ Atom "break" ]
  | Continue -> List [ Atom "continue" ]
  | Block l -> sexp_of_stmt_list l
  | Return l ->
      let rec aux k =
        match k with (_, e) :: t -> sexp_of_expr e :: aux t | [] -> []
      in
      List (Atom "return" :: aux l)
  | Assign (lhs, (_, r)) -> (
      match lhs with
      | Some (_, l) -> List [ Atom "="; sexp_of_expr l; sexp_of_expr r ]
      | None -> List [ Atom "="; blank_lhs; sexp_of_expr r ])
  | MultiAssign (enodelist, (_, r)) ->
      let rec aux l =
        match l with
        | Some (_, el) :: t -> sexp_of_expr el :: aux t
        | None :: t -> blank_lhs :: aux t
        | [] -> []
      in
      List [ Atom "="; List (aux enodelist); sexp_of_expr r ]
  | Declaration ((c, (_, dt), n), init) -> (
      let d =
        List
          [
            Atom n; (if c then Atom "const" else Atom "()"); sexp_of_datatype dt;
          ]
      in
      match init with
      | Some (_, e) -> List [ Atom "="; d; sexp_of_expr e ]
      | None -> d)
  | MultiDeclaration (c, (_, dt), vnlist) ->
      let names = List.fold_left (fun sl (_, x) -> Atom x :: sl) [] vnlist in
      List
        (names
        @ [ (if c then Atom "const" else Atom "()"); sexp_of_datatype dt ])
  | ArrayInit ((_, dt), elist, n) ->
      let rec cons_dim base nodims exprs =
        if nodims = 0 then sexp_of_datatype base
        else
          match exprs with
          | (_, h) :: t ->
              List [ Atom "[]"; cons_dim base (nodims - 1) t; sexp_of_expr h ]
          | [] -> List [ Atom "[]"; cons_dim base (nodims - 1) [] ]
      in
      let t =
        match dt with
        | Int i -> cons_dim (Int 0) i elist
        | Char i -> cons_dim (Char 0) i elist
        | Bool i -> cons_dim (Bool 0) i elist
        | NameType (slist, n, i) -> cons_dim (NameType (slist, n, 0)) i elist
      in
      List [ Atom "="; Atom n; t ]
  | ProcedureCall (slist, n, args) -> (
      match slist with
      | _ :: _ ->
          List
            [
              Atom ".";
              sexp_of_srclist slist;
              List (Atom "()" :: Atom n :: sexps_of_expr_list args);
            ]
      | [] -> List (Atom "()" :: Atom n :: sexps_of_expr_list args))
  | If ((_, exp), (_, tstmt), els) -> (
      match els with
      | Some (_, estmt) ->
          List
            [
              Atom "if";
              sexp_of_expr exp;
              sexp_of_stmt tstmt;
              sexp_of_stmt estmt;
            ]
      | None -> List [ Atom "if"; sexp_of_expr exp; sexp_of_stmt tstmt ])
  | While ((_, exp), (_, s)) ->
      List [ Atom "while"; sexp_of_expr exp; sexp_of_stmt s ]
  | For (init, cond, iter, (_, bod)) ->
      let fsexp tosf k =
        match k with Some (_, s) -> tosf s | None -> Atom "()"
      in
      List
        [
          Atom "for";
          fsexp sexp_of_stmt init;
          fsexp sexp_of_expr cond;
          fsexp sexp_of_stmt iter;
          sexp_of_stmt bod;
        ]

(* open Util *)

and sexp_of_stmt_list l =
  let rec aux k =
    match k with (_, s) :: t -> sexp_of_stmt s :: aux t | [] -> []
  in
  List (aux l)

let sexp_of_import (_, id) = List [ Atom "import"; Atom id ]

(* important note: stmt.Declaration does not use this *)
let sexp_of_decl (k : var_decl) =
  match k with
  | c, (_, dt), n ->
      if c then List [ Atom n; Atom "const"; sexp_of_datatype dt ]
      else List [ Atom n; sexp_of_datatype dt ]

let sexp_of_global = function
  | TypeDef (n, fields) ->
      List [ Atom n; List (List.map (fun (_, x) -> sexp_of_decl x) fields) ]
  | FunctionDef (function_decl, stmt_node_list) ->
      let name, decls, rets = function_decl in
      (* printing the function body (stmt^{*}) *)
      List
        [
          Atom name;
          List (List.map (fun (_, x) -> sexp_of_decl x) decls);
          List (List.map (fun (_, x) -> sexp_of_datatype x) rets);
          List
            (List.map (function _, stmt -> sexp_of_stmt stmt) stmt_node_list);
        ]
  | GlobalVarDecl (vdecl, ival) ->
      let c, (_, dt), n = vdecl in
      let b =
        sexp_of_datatype dt
        :: (match ival with Some (_, e) -> [ sexp_of_expr e ] | None -> [])
      in
      let res = if c then Atom "const" :: b else b in
      List (Atom ":global" :: Atom n :: res)

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

let rec print_sexp printer = function
  | Atom s -> Util.SexpPrinter.print_atom printer s
  | List sexp_list ->
      Util.SexpPrinter.(
        start_list printer ();
        List.iter (fun sexp -> print_sexp printer sexp) sexp_list;
        end_list printer ())
