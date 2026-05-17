open StdLabels

let rec match_pattern (value : Value.value) (pattern : Parsing.Ast.pat) env =
  match (pattern.pat_desc, value) with
  | Wildcard, _ -> Some env
  | Var pname, value -> Some (Value.Env.add pname value env)
  | Unit, Unit -> Some env
  | Int p, Int i -> if Int.equal p i then Some env else None
  | Bool p, Bool b -> if Bool.equal p b then Some env else None
  | Tuple pats, Tuple exprs ->
      let pat_count = List.length pats in
      if List.length exprs <> pat_count then None
      else
        let new_env =
          List.fold_left2 exprs pats ~init:(Some env) ~f:(fun env value pat ->
              Option.bind env @@ fun env ->
              match match_pattern value pat env with
              | None -> None
              | Some new_env ->
                  let merged_env =
                    Value.Env.union (fun _ _ _ -> None) env new_env
                  in
                  if
                    Value.Env.(
                      List.length (bindings merged_env)
                      = List.length (bindings env)
                        + List.length (bindings new_env))
                  then Some merged_env
                  else None)
        in
        new_env
  | _, _ -> None
