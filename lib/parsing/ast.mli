type expr = { expr_desc : expr_desc; loc : Location.t }
and binary_arith_op = Add | Sub | Mul | Div
and comp_op = Eq | Neq | Lt | Le | Gt | Ge

and expr_desc =
  | Var of string
  | Int of int
  | Bool of bool
  | Unit
  | Tuple of expr list
  | Arith of binary_arith_op * expr * expr
  | Comp of comp_op * expr * expr
  | If of expr * expr * expr
  | LetRec of pat * expr * expr
  | Let of pat * expr * expr
  | App of expr * expr
  | Lambda of string * expr
  | Match of expr * (pat * expr) list

and pat = { pat_desc : pat_desc; loc : Location.t }

and pat_desc =
  | Wildcard
  | Unit
  | Var of string
  | Int of int
  | Bool of bool
  | Tuple of pat list

val pp_expr : expr Fmt.t
