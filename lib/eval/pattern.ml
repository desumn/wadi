open StdLabels

let rec match_pattern env (pattern : Parsing.Ast.pat) (value : Value.value) =
  match (pattern.pat_desc, value) with
  | Wildcard, _ -> Some env
  | Var pname, value -> Some (Value.Env.add pname value env)
  | Unit, Unit -> Some env
  | Int p, Int i -> if Int.equal p i then Some env else None
  | Bool p, Bool b -> if Bool.equal p b then Some env else None
  | Tuple pats, Tuple values ->
      if List.(length pats <> length values) then None
      else
        List.fold_left2 ~init:(Some env)
          ~f:(fun env_opt pat value ->
            Option.bind env_opt (fun env -> match_pattern env pat value))
          pats values
  | Constr (pname, vpat), Constr (name, value) ->
      begin match (vpat, value) with
      | None, None -> Some env
      | Some _, None -> None
      | None, Some _ -> None
      | Some pat, Some value -> match_pattern env pat value
      end
  | _, _ -> None
