type expr = { desc : expr_desc; loc : Location.t }
and binary_arith_op = Add | Sub | Mul | Div
and comp_op = Eq | Neq | Lt | Le | Gt | Ge

and expr_desc =
  | Var of string
  | Int of int
  | Bool of bool
  | Arith of binary_arith_op * expr * expr
  | Comp of comp_op * expr * expr
  | If of expr * expr * expr
  | LetRec of string * expr * expr
  | Let of string * expr * expr
  | App of expr * expr
  | Lambda of string * expr

val pp_expr : expr Fmt.t
