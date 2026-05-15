module Env = Map.Make (String)

let rec eval_expr env (expr : Parsing.Ast.expr) =
  let open Result.Syntax in
  match expr.desc with
  | Int i -> Ok i
  | Var name ->
    begin match Env.find_opt name env with
    | Some v -> Ok v
    | None -> Error (`Unbound_variable name) 
    end
  | Add (l, r) ->
    let* l = eval_expr env l in
    let+ r = eval_expr env r in
    l + r
  | Sub (l, r) ->
    let* l = eval_expr env l in
    let+ r = eval_expr env r in
    l - r
  | Mul (l, r) ->
    let* l = eval_expr env l in
    let+ r = eval_expr env r in
    l * r
  | Div (l, r) ->
    let* l = eval_expr env l in
    let* r = eval_expr env r in
    if r = 0 then Error `Divide_by_zero
    else Ok (l / r)
  | Let (name, value, body) ->
    let* value = eval_expr env value in
    eval_expr (Env.add name value env) body
