type expr = { desc : expr_desc; loc : Location.t }

and expr_desc =
  | Var of string
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr * bool
  | App of expr * expr
  | Lambda of string * expr

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
  | Add (l, r) ->
      paren_if (level > add_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a +@ %a@]" (pp_expr_at add_level) l
        (pp_expr_at (add_level + 1))
        r
  | Sub (l, r) ->
      paren_if (level > sub_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a -@ %a@]" (pp_expr_at sub_level) l
        (pp_expr_at (sub_level + 1))
        r
  | Mul (l, r) ->
      paren_if (level > mul_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a *@ %a@]" (pp_expr_at mul_level) l
        (pp_expr_at (mul_level + 1))
        r
  | Div (l, r) ->
      paren_if (level > div_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a /@ %a@]" (pp_expr_at div_level) l
        (pp_expr_at (div_level + 1))
        r
  | Let (name, value, body, is_rec) ->
      let rec_s = if is_rec then "rec" else "" in
      Fmt.pf ppf "@[<2>let %s %s = %a@ in@ %a@]" rec_s name (pp_expr_at 0) value
        (pp_expr_at 0) body
  | App (func, value) ->
      paren_if (level > app_level) ppf @@ fun () ->
      Fmt.pf ppf "@[<2>%a@ %a@]" (pp_expr_at (app_level + 1)) func (pp_expr_at app_level) value
  | Lambda (arg, body) ->
      paren_if (level > lam_level) ppf @@ fun () ->
      Fmt.pf ppf "@[%s %a@]" arg (pp_expr_at lam_level) body

let pp_expr = pp_expr_at 0
