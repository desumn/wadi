module Env = Map.Make (String)

type eval_error = Divide_by_zero | Unbound_variable of string | Invalid_application | Int_type_error

let pp_error ppf eval_error =
  match eval_error with
  | Divide_by_zero -> Fmt.string ppf "divide by zero"
  | Unbound_variable var -> Fmt.pf ppf "unbound variable: %s" var
  | Invalid_application -> Fmt.string ppf "invalid application"
  | Int_type_error -> Fmt.string ppf "int type error"

let equal_error err1 err2 =
  match (err1, err2) with
  | Divide_by_zero, Divide_by_zero -> true
  | Unbound_variable var1, Unbound_variable var2 -> String.equal var1 var2
  | Invalid_application, Invalid_application -> true
  | Int_type_error, Int_type_error -> true
  | _ -> false

type value =
  | Int of int
  | Closure of {param: string; body : Parsing.Ast.expr; env : value Env.t; name : string option}

let pp_value ppf value =
  match value with
  | Int i -> Fmt.int ppf i
  | Closure {param; _} -> Fmt.pf ppf "closure: %s -> ..." param

let rec equal_value value1 value2 =
  match value1, value2 with
  | Int i1, Int i2 -> Int.equal i1 i2
  | Closure c1, Closure c2 -> Option.equal (String.equal) c1.name c2.name
  | _ -> false

let as_int value =
  match value with
  | Int i -> Ok i
  | _ -> Error Int_type_error

let as_int' res = Result.bind res as_int

let rec eval_expr env (expr : Parsing.Ast.expr) =
  let open Result.Syntax in
  match expr.desc with
  | Int i -> Ok (Int i)
  | Var name ->
      begin match Env.find_opt name env with
      | Some v -> Ok v
      | None -> Error (Unbound_variable name)
      end
  | Add (l, r) ->
      let* l = as_int' @@ eval_expr env l in
      let+ r = as_int' @@ eval_expr env r in
      Int(l + r)
  | Sub (l, r) ->
      let* l = as_int' @@ eval_expr env l in
      let+ r = as_int' @@ eval_expr env r in
      Int(l - r)
  | Mul (l, r) ->
      let* l = as_int' @@ eval_expr env l in
      let+ r = as_int' @@ eval_expr env r in
      Int(l * r)
  | Div (l, r) ->
      let* l = as_int' @@ eval_expr env l in
      let* r = as_int' @@ eval_expr env r in
      if r = 0 then Error Divide_by_zero else Ok (Int(l / r))
  | Let (name, value, body, is_rec) ->
      let* value = eval_expr env value in
      let new_env = match value with
      | Closure closure when is_rec -> Env.add name (Closure {closure with name=Some name}) env
      | _ -> Env.add name value env
      in
      eval_expr new_env body
  | App (on, value) ->
      let* on = eval_expr env on in
      let* value = eval_expr env value in
      begin match on with
      | Closure {param; body; env=capt_env; name} as closure ->
          let capt_env = begin match name with
          | Some name -> Env.add name closure capt_env
          | None -> capt_env
          end in
          eval_expr (Env.add param value capt_env) body
      | _ -> Error Invalid_application
      end
  | Lambda (param, body) -> Ok (Closure {param; body; env; name=None})
