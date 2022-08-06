open AstNode
open Core

let blank_lhs = Sexp.Atom "_"

let sexp_of_binop = function
  | Add -> Sexp.Atom "+"
  | Sub -> Sexp.Atom "-"
  | Mul -> Sexp.Atom "*"
  | Div -> Sexp.Atom "/"
  | Mod -> Sexp.Atom "%"
  | Lte -> Sexp.Atom "<="
  | Gte -> Sexp.Atom ">="
  | Lt -> Sexp.Atom "<"
  | Gt -> Sexp.Atom ">"
  | Neq -> Sexp.Atom "!="
  | Deq -> Sexp.Atom "=="
  | BinOr -> Sexp.Atom "|"
  | BinAnd -> Sexp.Atom "&"
  | Or -> Sexp.Atom "||"
  | And -> Sexp.Atom "&&"

let sexp_of_unop = function
  | Neg -> Sexp.Atom "-"
  | Not -> Sexp.Atom "!"
  | BinNot -> Sexp.Atom "~"

let rec sexp_of_srclist k =
  match k with
  | [ (_, s) ] -> Sexp.Atom s
  | [ (_, s1); (_, s2) ] ->
      Sexp.List [ Sexp.Atom "."; Sexp.Atom s1; Sexp.Atom s2 ]
  | (_, s) :: t -> Sexp.List [ Sexp.Atom "."; sexp_of_srclist t; Sexp.Atom s ]
  | [] -> Sexp.Atom ""

let n_lrsbrac n =
  let rec aux k acc = if k = 0 then acc else aux (k - 1) acc ^ "[]" in
  aux n ""

let sexp_of_datatype dt =
  let rec tformat t d =
    match d with
    | 0 -> t
    (* | 1 -> Sexp.List [ Sexp.Atom "[]"; t ] *)
    | _ -> tformat (Sexp.List [ Sexp.Atom "[]"; t ]) (d - 1)
  in
  match dt with
  | Int i -> tformat (Sexp.Atom "int") i
  | Char i -> tformat (Sexp.Atom "char") i
  | Bool i -> tformat (Sexp.Atom "bool") i
  | NameType (slist, n, i) ->
      let k =
        match slist with
        | _ :: _ ->
            Sexp.List [ Sexp.Atom "."; sexp_of_srclist slist; Sexp.Atom n ]
        | _ -> Sexp.Atom n
      in
      tformat k i

let rec sexp_of_expr = function
  | BinopExpr (o, (_, e1), (_, e2)) ->
      Sexp.List [ sexp_of_binop o; sexp_of_expr e1; sexp_of_expr e2 ]
  | UnaryExpr (o, (_, e)) -> Sexp.List [ sexp_of_unop o; sexp_of_expr e ]
  | IntLiteral i -> Sexp.Atom (Int64.to_string i)
  | StrLiteral s ->
      Sexp.Atom (Printf.sprintf "\"%s\"" (Util.Unicode.string_of_intq s))
  | CharLiteral c -> Sexp.Atom (Int.to_string c)
  | BoolLiteral b -> Sexp.Atom (if b then "true" else "false")
  | ArrayLiteral l ->
      Sexp.List
        (let rec pelist elist =
           match elist with
           | (_, e) :: t -> sexp_of_expr e :: pelist t
           | [] -> []
         in
         pelist l)
  | Var s -> Sexp.Atom s
  | FieldAccess ((_, e), s) ->
      Sexp.List [ Sexp.Atom "."; sexp_of_expr e; Sexp.Atom s ]
  | ArrayAccess ((_, e1), (_, e2)) ->
      Sexp.List [ Sexp.Atom "[]"; sexp_of_expr e1; sexp_of_expr e2 ]
  | ModuleAccess (slist, s) ->
      Sexp.List [ Sexp.Atom "moduleAccess"; sexp_of_srclist slist; Sexp.Atom s ]
  | Cast ((_, dt), (_, e)) ->
      Sexp.List [ Sexp.Atom "cast"; sexp_of_datatype dt; sexp_of_expr e ]
  | FunctionCall (slist, n, alist) -> (
      match slist with
      | _ :: _ ->
          Sexp.List
            [
              Sexp.Atom ".";
              sexp_of_srclist slist;
              Sexp.List (Sexp.Atom n :: [ Sexp.List (sexps_of_expr_list alist) ]);
            ]
      | _ -> Sexp.List (Sexp.Atom n :: [ Sexp.List (sexps_of_expr_list alist) ])
      )
  | ConstructorCall (slist, n, named_args) -> (
      match slist with
      | _ :: _ ->
          Sexp.List
            [
              Sexp.Atom ".";
              sexp_of_srclist slist;
              Sexp.List
                (Sexp.Atom n :: [ Sexp.List (named_args_to_sexp named_args) ]);
            ]
      | _ ->
          Sexp.List
            (Sexp.Atom n :: [ Sexp.List (named_args_to_sexp named_args) ]))
  | Null -> Sexp.Atom "null"

and sexps_of_expr_list elist =
  let rec aux l =
    match l with (_, e) :: t -> sexp_of_expr e :: aux t | [] -> []
  in
  aux elist

and named_args_to_sexp elist =
  let rec aux l =
    match l with
    | (_, n, (_, e)) :: t ->
        Sexp.List [ Sexp.Atom "="; Sexp.Atom n; sexp_of_expr e ] :: aux t
    | [] -> []
  in
  aux elist

let rec sexp_of_stmt = function
  | Break -> Sexp.List [ Sexp.Atom "break" ]
  | Continue -> Sexp.List [ Sexp.Atom "continue" ]
  | Block l -> sexp_of_stmt_list l
  | Return l ->
      let rec aux k =
        match k with (_, e) :: t -> sexp_of_expr e :: aux t | [] -> []
      in
      Sexp.List (Sexp.Atom "return" :: aux l)
  | Assign (lhs, (_, r)) -> (
      match lhs with
      | Some (_, l) ->
          Sexp.List [ Sexp.Atom "="; sexp_of_expr l; sexp_of_expr r ]
      | None -> Sexp.List [ Sexp.Atom "="; blank_lhs; sexp_of_expr r ])
  | MultiAssign (enodelist, (_, r)) ->
      let rec aux l =
        match l with
        | Some (_, el) :: t -> sexp_of_expr el :: aux t
        | None :: t -> blank_lhs :: aux t
        | [] -> []
      in
      Sexp.List [ Sexp.Atom "="; Sexp.List (aux enodelist); sexp_of_expr r ]
  | Declaration ((c, (_, dt), n), init) -> (
      let base =
        if c then Sexp.Atom "const" :: [ sexp_of_datatype dt ]
        else [ sexp_of_datatype dt ]
      in
      let d = Sexp.List (Sexp.Atom n :: base) in
      match init with
      | Some (_, e) -> Sexp.List [ Sexp.Atom "="; d; sexp_of_expr e ]
      | None -> d)
  | MultiDeclaration (c, (_, dt), vnlist) ->
      let names = Stdlib.List.map (fun (_, n) -> Sexp.Atom n) vnlist in
      Sexp.List
        (names
        @
        if c then Sexp.Atom "const" :: [ sexp_of_datatype dt ]
        else [ sexp_of_datatype dt ])
  | ArrayInit ((_, dt), elist, n) ->
      let rec cons_dim base nodims exprs =
        if nodims = 0 then sexp_of_datatype base
        else
          match exprs with
          | (_, h) :: t ->
              Sexp.List
                [ Sexp.Atom "[]"; cons_dim base (nodims - 1) t; sexp_of_expr h ]
          | [] -> Sexp.List [ Sexp.Atom "[]"; cons_dim base (nodims - 1) [] ]
      in
      let t =
        match dt with
        | Int i -> cons_dim (Int 0) i elist
        | Char i -> cons_dim (Char 0) i elist
        | Bool i -> cons_dim (Bool 0) i elist
        | NameType (slist, n, i) -> cons_dim (NameType (slist, n, 0)) i elist
      in
      Sexp.List [ Sexp.Atom "="; Sexp.Atom n; t ]
  | ProcedureCall (slist, n, args) -> (
      match slist with
      | _ :: _ ->
          Sexp.List
            [
              Sexp.Atom ".";
              sexp_of_srclist slist;
              Sexp.List (Sexp.Atom n :: [ Sexp.List (sexps_of_expr_list args) ]);
            ]
      | [] -> Sexp.List (Sexp.Atom n :: [ Sexp.List (sexps_of_expr_list args) ])
      )
  | If ((_, exp), (_, tstmt), els) -> (
      match els with
      | Some (_, estmt) ->
          Sexp.List
            [
              Sexp.Atom "if";
              sexp_of_expr exp;
              sexp_of_stmt tstmt;
              sexp_of_stmt estmt;
            ]
      | None ->
          Sexp.List
            [
              Sexp.Atom "if"; sexp_of_expr exp; sexp_of_stmt tstmt; Sexp.List [];
            ])
  | While ((_, exp), (_, s)) ->
      Sexp.List [ Sexp.Atom "while"; sexp_of_expr exp; sexp_of_stmt s ]
  | For (init, cond, iter, (_, bod)) ->
      let fsexp tosf k =
        match k with Some (_, s) -> tosf s | None -> Sexp.List []
      in
      Sexp.List
        [
          Sexp.Atom "for";
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
  Sexp.List (aux l)

let sexp_of_import (_, id) = Sexp.List [ Atom "import"; Atom id ]

(* important note: stmt.Declaration does not use this *)
let sexp_of_decl (k : var_decl) =
  match k with
  | c, (_, dt), n ->
      if c then
        Sexp.List [ Sexp.Atom n; Sexp.Atom "const"; sexp_of_datatype dt ]
      else Sexp.List [ Sexp.Atom n; sexp_of_datatype dt ]

let sexp_of_global = function
  | TypeDef (n, fields) ->
      Sexp.List
        [
          Sexp.Atom n;
          Sexp.List (Stdlib.List.map (fun (_, x) -> sexp_of_decl x) fields);
        ]
  | FunctionDef (function_decl, stmt_node_list) ->
      let name, decls, rets = function_decl in
      (* printing the function body (stmt^{*}) *)
      Sexp.List
        [
          Sexp.Atom name;
          Sexp.List (Stdlib.List.map (fun (_, x) -> sexp_of_decl x) decls);
          Sexp.List (Stdlib.List.map (fun (_, x) -> sexp_of_datatype x) rets);
          Sexp.List
            (Stdlib.List.map
               (function _, stmt -> sexp_of_stmt stmt)
               stmt_node_list);
        ]
  | GlobalVarDecl (vdecl, ival) ->
      let c, (_, dt), n = vdecl in
      let b =
        sexp_of_datatype dt
        :: (match ival with Some (_, e) -> [ sexp_of_expr e ] | None -> [])
      in
      let res = if c then Sexp.Atom "const" :: b else b in
      Sexp.List (Sexp.Atom ":global" :: Sexp.Atom n :: res)

let sexp_of_file = function
  | Interface -> failwith "todo sexp interface"
  | Module (imports, global_items) ->
      let import_sexp =
        Sexp.List
          (Stdlib.List.map (fun import -> sexp_of_import import) imports)
      in
      let globals_sexp =
        Sexp.List
          (Stdlib.List.map
             (fun (_, global) -> sexp_of_global global)
             global_items)
      in
      Sexp.List [ import_sexp; globals_sexp ]

let rec print_sexp printer = function
  | Sexp.Atom s -> Util.SexpPrinter.print_atom printer s
  | Sexp.List sexp_list ->
      Util.SexpPrinter.(
        start_list printer ();
        Stdlib.List.iter (fun sexp -> print_sexp printer sexp) sexp_list;
        end_list printer ())
