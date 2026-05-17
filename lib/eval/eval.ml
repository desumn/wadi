open StdLabels
module Value = Value
module Pattern = Pattern
open Value

type eval_error =
  | Divide_by_zero
  | Unbound_variable of string
  | Invalid_application
  | Type_mismatch of (expected:string * value)
  | No_pattern_found
  | Invalid_rec

let pp_error ppf eval_error =
  match eval_error with
  | Divide_by_zero -> Fmt.string ppf "divide by zero"
  | Unbound_variable var -> Fmt.pf ppf "unbound variable: %s" var
  | Invalid_application -> Fmt.string ppf "invalid application"
  | Type_mismatch (~expected, value) ->
      Fmt.pf ppf "type mismatch: expected %s, got %a" expected pp_value value
  | No_pattern_found -> Fmt.string ppf "no pattern found"
  | Invalid_rec -> Fmt.string ppf "invalid rec"

let equal_error err1 err2 =
  match (err1, err2) with
  | Divide_by_zero, Divide_by_zero -> true
  | Unbound_variable var1, Unbound_variable var2 -> String.equal var1 var2
  | Invalid_application, Invalid_application -> true
  | ( Type_mismatch (~expected:expected1, _),
      Type_mismatch (~expected:expected2, _) ) ->
      String.equal expected1 expected2
  | No_pattern_found, No_pattern_found -> true
  | Invalid_rec, Invalid_rec -> true
  | _ -> false

let as_int value =
  match value with
  | Int i -> Ok i
  | _ -> Error (Type_mismatch (~expected:"int", value))

let as_int' res = Result.bind res as_int

let as_bool value =
  match value with
  | Bool b -> Ok b
  | _ -> Error (Type_mismatch (~expected:"bool", value))

let as_bool' res = Result.bind res as_bool

let as_unit value =
  match value with
  | Unit -> Ok ()
  | _ -> Error (Type_mismatch (~expected:"unit", value))

let as_unit' res = Result.bind res as_unit

let as_tuple value =
  match value with
  | Tuple exprs -> Ok exprs
  | _ -> Error (Type_mismatch (~expected:"tuple", value))

let as_tuple' res = Result.bind res as_tuple

let comp_fun : Parsing.Ast.comp_op -> 'a -> 'a -> bool = function
  | Eq -> ( = )
  | Neq -> ( <> )
  | Lt -> ( < )
  | Le -> ( <= )
  | Gt -> ( > )
  | Ge -> ( >= )

let rec eval_expr' env (expr : Parsing.Ast.expr) =
  let open Result.Syntax in
  match expr.expr_desc with
  | Int i -> Ok (Int i)
  | Bool i -> Ok (Bool i)
  | Unit -> Ok Unit
  | Tuple exprs ->
      let+ exprs =
        List.fold_right
          ~f:(fun res acc ->
            let* acc = acc in
            let+ res = res in
            res :: acc)
          ~init:(Ok [])
        @@ List.map ~f:(eval_expr' env) exprs
      in
      Tuple exprs
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
      Bool (comp_fun op l r)
  | If (cond, then_, else_) ->
      let* cond = as_bool' @@ eval_expr' env cond in
      if cond then eval_expr' env then_ else eval_expr' env else_
  | Let (pat, value, body) ->
      let* value = eval_expr' env value in
      let new_env = Pattern.match_pattern value pat env in
      begin match new_env with
      | Some new_env -> eval_expr' new_env body
      | None -> Error No_pattern_found
      end
  | LetRec (name, value, body) ->
      let* value = eval_expr' env value in
      let name =
        match name.pat_desc with Var name -> name | _ -> assert false
      in
      begin match value with
      | Closure closure ->
          let new_env =
            Env.add name (Closure { closure with name = Some name }) env
          in
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
  | Match (expr, pats) ->
      let* expr = eval_expr' env expr in
      begin match
        List.find_map pats ~f:(fun (pat, body) ->
            Option.map
              (fun env -> (env, body))
              (Pattern.match_pattern expr pat env))
      with
      | Some (env, body) -> eval_expr' env body
      | None -> Error No_pattern_found
      end

let builtins =
  Env.of_list
    [
      ( "not",
        Closure
          {
            param = "b";
            body = Parsing.parse_string "if b then false else true";
            env = Env.empty;
            name = None;
          } );
    ]

let eval_expr env = eval_expr' (Env.union (fun _ e b -> Some e) env builtins)
