module Env = Map.Make (String)


type value =
  | Int of int
  | Bool of bool
  | Closure of {
      param : string;
      body : Parsing.Ast.expr;
      env : value Env.t;
      name : string option;
    }

let pp_value ppf value =
  match value with
  | Int i -> Fmt.int ppf i
  | Bool b -> Fmt.bool ppf b
  | Closure { param; _ } -> Fmt.pf ppf "closure: %s -> ..." param

let rec equal_value value1 value2 =
  match (value1, value2) with
  | Int i1, Int i2 -> Int.equal i1 i2
  | Closure c1, Closure c2 -> Option.equal String.equal c1.name c2.name
  | Bool b1, Bool b2 -> Bool.equal b1 b2
  | _ -> false

type eval_error =
  | Divide_by_zero
  | Unbound_variable of string
  | Invalid_application
  | Type_mismatch of (expected:string * value)
  | Invalid_rec

let pp_error ppf eval_error =
  match eval_error with
  | Divide_by_zero -> Fmt.string ppf "divide by zero"
  | Unbound_variable var -> Fmt.pf ppf "unbound variable: %s" var
  | Invalid_application -> Fmt.string ppf "invalid application"
  | Type_mismatch ~expected, value -> Fmt.pf ppf "type mismatch: expected %s, got %a" expected pp_value value
  | Invalid_rec -> Fmt.string ppf "invalid rec"

let equal_error err1 err2 =
  match (err1, err2) with
  | Divide_by_zero, Divide_by_zero -> true
  | Unbound_variable var1, Unbound_variable var2 -> String.equal var1 var2
  | Invalid_application, Invalid_application -> true
  | Type_mismatch (~expected:expected1,_), Type_mismatch (~expected:expected2, _) ->
      String.equal expected1 expected2
  | Invalid_rec, Invalid_rec -> true
  | _ -> false

let as_int value = match value with Int i -> Ok i | _ -> Error (Type_mismatch (~expected:"int", value))
let as_int' res = Result.bind res as_int

let as_bool value = match value with Bool b -> Ok b | _ -> Error (Type_mismatch (~expected:"bool", value))
let as_bool' res = Result.bind res as_bool

let comp_fun : Parsing.Ast.comp_op -> ('a -> 'a -> bool) =
  function
  | Eq -> (=)
  | Neq -> (<>)
  | Lt -> (<)
  | Le -> (<=)
  | Gt -> (>)
  | Ge -> (>=)

let rec eval_expr' env (expr : Parsing.Ast.expr) =
  let open Result.Syntax in
  match expr.desc with
  | Int i -> Ok (Int i)
  | Bool i -> Ok (Bool i)
  | Var name ->
      begin match Env.find_opt name env with
      | Some v -> Ok v
      | None -> Error (Unbound_variable name)
      end      
  | Arith (Add, l, r) ->
      let* l = as_int' @@ eval_expr' env l in
      let+ r = as_int' @@ eval_expr' env r in
      Int (l + r)
  | Arith (Sub, l, r) ->
      let* l = as_int' @@ eval_expr' env l in
      let+ r = as_int' @@ eval_expr' env r in
      Int (l - r)
  | Arith (Mul, l, r) ->
      let* l = as_int' @@ eval_expr' env l in
      let+ r = as_int' @@ eval_expr' env r in
      Int (l * r)
  | Arith (Div, l, r) ->
      let* l = as_int' @@ eval_expr' env l in
      let* r = as_int' @@ eval_expr' env r in
      if r = 0 then Error Divide_by_zero else Ok (Int (l / r))
  | Comp (op, l, r) ->
      let* l = eval_expr' env l in
      let+ r = eval_expr' env r in
      Bool(comp_fun op l r)
  | If (cond, then_, else_) ->
      let* cond = as_bool' @@ eval_expr' env cond in
      if cond then
        eval_expr' env then_
      else
        eval_expr' env else_
  | Let (name, value, body) ->
      let* value = eval_expr' env value in
      let new_env =  Env.add name value env
      in
      eval_expr' new_env body
  | LetRec (name, value, body) ->
      let* value = eval_expr' env value in
      begin match value with
        | Closure closure ->
          let new_env = Env.add name (Closure { closure with name = Some name }) env in
          eval_expr' new_env body
        | _ -> Error Invalid_rec
      end
  | App (on, value) ->
      let* on = eval_expr' env on in
      let* value = eval_expr' env value in
      begin match on with
      | Closure { param; body; env = capt_env; name } as closure ->
          let capt_env =
            begin match name with
            | Some name -> Env.add name closure capt_env
            | None -> capt_env
            end
          in
          eval_expr' (Env.add param value capt_env) body
      | _ -> Error Invalid_application
      end
  | Lambda (param, body) -> Ok (Closure { param; body; env; name = None })

let builtins =
  Env.of_list [
    ("not", Closure 
      { param = "b";
      body = Parsing.parse_string "if b then false else true" ;
      env = Env.empty;
      name = None; })
  ]

let eval_expr env = eval_expr' (Env.union (fun _ e b -> Some e) env builtins)
