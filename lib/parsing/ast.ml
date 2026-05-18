
type tuple_pat = { fst : pat; snd : pat; extra : pat list }
and constr_pat = { name : string; args : pat list }
and structural_pat =
  | WildcardPat
  | VarPat of string
  | IntPat of int
  | BoolPat of bool
  | UnitPat
  | TuplePat of tuple_pat
  | ConstrPat of constr_pat
and pat =
  | Structural of {structural_pat : structural_pat; loc : Location.t }

type tuple_lit = { fst : expr; snd : expr; extra : expr list }
and constr_lit = { name : string; args : expr list }
and literal =
  | VarLit of string
  | IntLit of int
  | BoolLit of bool
  | UnitLit
  | TupleLit of tuple_lit
  | ConstrLit of constr_lit

and unary_operator =
  | Neg
and unary_operation = {
  operator : unary_operator;
  operand : expr;
}

and binary_operator =
  | Add | Sub | Mul | Div
  | And | Or
  | Eq | Neq | Lt | Le | Gt | Ge
and binary_operation = {
  operator : binary_operator;
  left : expr;
  right : expr;
}

and if_expr = {
  cond : expr;
  then_ : expr;
  else_ : expr;
}

and match_expr = {
  scrutinee : expr;
  cases : (pat * expr) list
}

and lambda = {
  params : pat list;
  body : expr
}

and app = {
  left : expr;
  right : expr;
}

and let_destruct = {
  pattern : pat;
  value : expr;
}

and let_fun = {
  name : string;
  is_rec : bool;
  params : pat list;
  value : expr;
}

and let_expr =
  | LetDestruct of { let_destruct : let_destruct; body : expr }
  | LetFun of { let_fun : let_fun ; body : expr }

and expr =
  | Lit of { literal : literal; loc : Location.t }
  | UnaryOperation of { operation : unary_operation; loc : Location.t }
  | BinaryOperation of { operation : binary_operation; loc : Location.t }
  | If of { if_expr : if_expr; loc : Location.t }
  | Match of { match_expr : match_expr; loc : Location.t }
  | Lambda of { lambda : lambda; loc : Location.t }
  | App of { app : app; loc : Location.t }
  | Let of { let_expr : let_expr; loc : Location.t }

and let_top =
  | LetTopDestruct of let_destruct
  | LetTopFun of let_fun

and top =
  | ExprTop of expr
  | LetTop of { let_top : let_top; loc : Location.t }

and program = top list

let loc_of_expr expr =
  match expr with
  | Lit { loc }
  | UnaryOperation { loc }
  | BinaryOperation { loc }
  | If { loc }
  | Match { loc }
  | Lambda { loc }
  | App { loc }
  | Let { loc } -> loc
