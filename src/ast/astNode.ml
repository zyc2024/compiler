type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lte
  | Gte
  | Lt
  | Gt
  | Neq
  | Deq
  | BinOr
  | BinAnd
  | Or
  | And

type unop =
  | Neg
  | Not
  | BinNot

type dest_expr =
  | Var of string
  | FieldAccess of expr_node * string
  | ArrayAccess of expr_node * expr_node
  | ModuleAccess of (Lexing.position * string) list * string
  | Underscore

and expr =
  | BinopExpr of binop * expr_node * expr_node
  | UnaryExpr of unop * expr_node
  | IntLiteral of int64
  | StrLiteral of int Queue.t
  | CharLiteral of int
  | BoolLiteral of bool
  | ArrayLiteral of expr_node list
  | LhsExpr of dest_expr_node
  | Cast of data_type_node * expr_node
  | FunctionCall of (Lexing.position * string) list * string * expr_node list
  | ConstructorCall of
      (Lexing.position * string) list
      * string
      * (Lexing.position * string * expr_node) list
  | Null

and expr_node = Lexing.position * expr
and dest_expr_node = Lexing.position * dest_expr

(** [data_type] is (T, dim) with T representing possible base type. *)
and data_type =
  | Int of int
  | Char of int
  | Bool of int
  | NameType of (Lexing.position * string) list * string * int

and data_type_node = Lexing.position * data_type

type stmt =
  | Break
  | Continue
  | Block of stmt_node list
  | Return of expr_node list
  | Assign of dest_expr_node * expr_node
  | Declaration of bool * data_type_node * string * expr_node option
  | ProcedureCall of (Lexing.position * string) list * string * expr_node list
  | If of (expr_node * stmt_node * stmt_node Option.t)
  | While of (expr_node * stmt_node)

and stmt_node = Lexing.position * stmt

(* examples *)
