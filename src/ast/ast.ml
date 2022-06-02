type binOp = | ADD | SUB | MUL | DIV | MOD

type expr = 
  | BinopExpr of (binOp * expr * expr)
  | IntLiteral of int
  | Strliteral of string
  | CharLiteral of Uchar.t
  | BoolLiteral of bool
