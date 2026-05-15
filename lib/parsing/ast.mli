
type expr = {
  desc : expr_desc;
  loc : Location.t;
}
and expr_desc =
  | Var of string
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr

val pp_expr : expr Fmt.t
