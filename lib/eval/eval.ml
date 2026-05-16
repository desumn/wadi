module Env = Map.Make (String)

type eval_error = Divide_by_zero | Unbound_variable of string

let pp_error ppf eval_error =
  match eval_error with
  | Divide_by_zero -> Fmt.string ppf "divide by zero"
  | Unbound_variable var -> Fmt.pf ppf "unbound variable: %s" var

let equal_error err1 err2 =
  match (err1, err2) with
  | Divide_by_zero, Divide_by_zero -> true
  | Unbound_variable var1, Unbound_variable var2 -> String.equal var1 var2
  | _ -> false

let rec eval_expr env (expr : Parsing.Ast.expr) =
  let open Result.Syntax in
  match expr.desc with
  | Int i -> Ok i
  | Var name ->
      begin match Env.find_opt name env with
      | Some v -> Ok v
      | None -> Error (Unbound_variable name)
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
      if r = 0 then Error Divide_by_zero else Ok (l / r)
  | Let (name, value, body) ->
      let* value = eval_expr env value in
      eval_expr (Env.add name value env) body
