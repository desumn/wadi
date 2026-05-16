type expr = { desc : expr_desc; loc : Location.t }


and binary_arith_op =
  | Add | Sub | Mul | Div

and comp_op =
  | Eq | Neq | Lt | Le | Gt | Ge


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

let pp_binary_arith_op ppf op =
  match op with
  | Add -> Fmt.pf ppf "+"
  | Sub -> Fmt.pf ppf "-"
  | Mul -> Fmt.pf ppf "*"
  | Div -> Fmt.pf ppf "/"

let pp_comp_op ppf op =
  match op with
  | Eq -> Fmt.pf ppf "="
  | Neq -> Fmt.pf ppf "<>"
  | Lt -> Fmt.pf ppf "<"
  | Le -> Fmt.pf ppf "<="
  | Gt -> Fmt.pf ppf ">"
  | Ge -> Fmt.pf ppf ">="


let lam_level = 1
let add_level = 2
let sub_level = 3
let mul_level = 4
let div_level = 5
let app_level = 6

let paren_if cond ppf do_ =
  if cond then begin
    Fmt.pf ppf "(";
    do_ ();
    Fmt.pf ppf ")"
  end
  else do_ ()

let rec pp_expr_at level ppf expr =
  match expr.desc with
  | Var name -> Fmt.string ppf name
  | Int int -> Fmt.int ppf int
  | Bool bool -> Fmt.bool ppf bool
  | Arith (Add, l, r) ->
      paren_if (level > add_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a +@ %a@]" (pp_expr_at add_level) l
        (pp_expr_at (add_level + 1))
        r
  | Arith (Sub, l, r) ->
      paren_if (level > sub_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a -@ %a@]" (pp_expr_at sub_level) l
        (pp_expr_at (sub_level + 1))
        r
  | Arith (Mul, l, r) ->
      paren_if (level > mul_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a *@ %a@]" (pp_expr_at mul_level) l
        (pp_expr_at (mul_level + 1))
        r
  | Arith (Div, l, r) ->
      paren_if (level > div_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a /@ %a@]" (pp_expr_at div_level) l
        (pp_expr_at (div_level + 1))
        r
  | Comp (comp_op, l, r) ->
      Fmt.pf ppf "@[<2>%a %a@ %a@]" (pp_expr_at (level + 1)) l pp_comp_op comp_op (pp_expr_at level) r
  | If (cond, then_, else_) ->
      Fmt.pf ppf "@[<2>if %a then@ %a else@ %a@]" (pp_expr_at 0) cond (pp_expr_at 0) then_
        (pp_expr_at 0) else_
  | Let (name, value, body) ->
      Fmt.pf ppf "@[<2>let %s = %a@ in@ %a@]" name (pp_expr_at 0) value
        (pp_expr_at 0) body
  | LetRec (name, value, body) ->
      Fmt.pf ppf "@[<2>let rec %s = %a@ in@ %a@]" name (pp_expr_at 0) value
        (pp_expr_at 0) body
  | App (func, value) ->
      paren_if (level > app_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a@ %a@]"
        (pp_expr_at (app_level + 1))
        func (pp_expr_at app_level) value
  | Lambda (arg, body) ->
      paren_if (level > lam_level) ppf @@ fun () ->
      Fmt.pf ppf "@[%s %a@]" arg (pp_expr_at lam_level) body

let pp_expr = pp_expr_at 0
