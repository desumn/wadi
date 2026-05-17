open StdLabels

let rec match_pattern (value : Value.value) (pattern : Parsing.Ast.pat) env =
  match (pattern.pat_desc, value) with
  | Wildcard, _ -> Some env
  | Var pname, value -> Some (Value.Env.add pname value env)
  | Unit, Unit -> Some env
  | Int p, Int i -> if Int.equal p i then Some env else None
  | Bool p, Bool b -> if Bool.equal p b then Some env else None
  | Tuple pats, Tuple values ->
      List.fold_left2 ~init:(Some env)
        ~f:(fun env_opt pat value ->
          Option.bind env_opt (fun env -> match_pattern value pat env))
        pats values
  | _, _ -> None
