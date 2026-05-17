module Env = Map.Make (String)
open StdLabels

type value =
  | Int of int
  | Bool of bool
  | Unit
  | Tuple of value list
  | Constr of string * value option
  | Closure of {
      param : string;
      body : Parsing.Ast.expr;
      env : value Env.t;
      name : string option;
    }

let rec pp_value ppf value =
  match value with
  | Int i -> Fmt.int ppf i
  | Bool b -> Fmt.bool ppf b
  | Unit -> Fmt.pf ppf "()"
  | Tuple exprs -> Fmt.pf ppf "(%a)" (Fmt.list pp_value ~sep:Fmt.comma) exprs
  | Constr (name, value) -> Fmt.pf ppf "%s %a" name (Fmt.option pp_value) value
  | Closure { param; _ } -> Fmt.pf ppf "closure: %s -> ..." param

let rec equal_value value1 value2 =
  match (value1, value2) with
  | Int i1, Int i2 -> Int.equal i1 i2
  | Bool b1, Bool b2 -> Bool.equal b1 b2
  | Unit, Unit -> true
  | Tuple exprs1, Tuple exprs2 -> List.equal ~eq:equal_value exprs1 exprs2
  | Constr (name1, value1), Constr (name2, value2) ->
      String.equal name1 name2 && Option.equal equal_value value1 value2
  | Closure c1, Closure c2 -> Option.equal String.equal c1.name c2.name
  | _ -> false
