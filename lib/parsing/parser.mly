%{
open StdLabels
open Ast

let located_bool b loc = { expr_desc = Bool b; loc = Location.make loc }


%}
%token <int> Int
%token <string> Lident
%token <string> Uident
%token <bool> Bool

%token Comma ","

%token ParenOpen "(" ParenClose ")"

%token Plus "+" Minus "-" Star "*" Slash "/" 

%token Equal "=" 

%token NotEqual "<>" Less "<" LessEqual "<=" Greater ">" GreaterEqual ">="

%token Bar "|" Underscore "_"

%token And Or

%token Let Rec In

%token Match With

%token If Then Else

%token Fun Arrow "->"

%token Eof

%start <expr> program

%%

let located_expr(X) ==
  | x = X;  { { expr_desc = x; loc = Location.make $loc } }

let located_pat(X) ==
  | x = X;  { { pat_desc = x; loc = Location.make $loc } }

let program :=
  | ~ = located_expr(expr); Eof; <>

let expr :=
  | ~ = open_expr; <>
  | ~ = logical_or; <>

let open_expr :=
  | Let; pat = located_pat(pat); "=";
    value = located_expr(expr); In; body = located_expr(expr);
    { Let (pat, value, body) }
  | Let; name = Lident; arg1 = Lident; args = Lident*; "=";
    value = located_expr(expr); In; body = located_expr(expr);
    {
      let args = arg1 :: args in
      let value = List.fold_right args ~init:value
        ~f:(fun arg value -> {value with expr_desc = Lambda(arg, value)}) in
      let pat = { pat_desc = Var name; loc = Location.make $loc } in
      Let (pat, value, body)
    }
  | Let; Rec; name = Lident; args = Lident*; "=";
    value = located_expr(expr); In; body = located_expr(expr);
    {
      let value = List.fold_right args ~init:value
        ~f:(fun arg value -> {value with expr_desc = Lambda(arg, value)}) in
      LetRec (name, value, body)
    }
  | Fun; param = Lident; "->"; body = located_expr(expr); <Lambda>
  | If; cond = located_expr(expr); Then; then_ = located_expr(expr); Else; else_ = located_expr(expr); <If>
  | Match; e = located_expr(expr); With; bs = match_branches;
    { Match (e, bs) }

let match_branches :=
  | "|"?; b = match_branch; bs = list("|"; b = match_branch; <>);
    { b :: bs }
let match_branch :=
  | p = located_pat(pat); "->"; e = located_expr(expr); { (p, e) }
let pat :=
  | ~ = atomic_pat; <>
  | "("; p = located_pat(pat); ","; ps = separated_nonempty_list(",", located_pat(pat)); ")";
    { Tuple (p :: ps) }
let atomic_pat :=
  | "_"; { Wildcard }
  | name = Lident; <Var>
  | n = Int; <Int>
  | b = Bool; <Bool>
  | "("; ")"; { Unit }
  | "("; ~ = pat; ")"; <>

let logical_or :=
  | l = located_expr(logical_or); Or; r = located_expr(logical_and);
    { If (l, located_bool true $loc, r) }
  | ~ = logical_and; <>
let logical_and :=
  | l = located_expr(logical_and); And; r = located_expr(comparison);
    { If (l, r, located_bool false $loc) }
  | ~ = comparison; <>

%inline comp_op:
  | "=" {Eq}
  | "<>" {Neq}
  | "<" {Lt}
  | "<=" {Le}
  | ">" {Gt}
  | ">=" {Ge}

let comparison :=
  | l = located_expr(additive); op = comp_op; r = located_expr(additive); {Comp (op, l, r)}
  | ~ = additive; <>

%inline binop_add:
  | "+"; { Add }
  | "-"; { Sub }
%inline binop_mul:
  | "*"; { Mul }
  | "/"; { Div }

let additive :=
  | l = located_expr(additive); op = binop_add; r = located_expr(multiplicative);
    { Arith (op, l, r) }
  | ~ = multiplicative; <>

let multiplicative :=
  | l = located_expr(multiplicative); op = binop_mul; r = located_expr(app_expr);
    { Arith (op, l, r) }
  | ~ = app_expr; <>

let app_expr :=
  | f = located_expr(app_expr); arg = located_expr(atom_expr); <App>
  | ~ = atom_expr; <>

let atom_expr :=
  | name = Lident; <Var>
  | num = Int; <Int>
  | bool = Bool; <Bool>
  | "("; ")"; {Unit}
  | "("; e = expr; ")"; <>
  | "("; e = located_expr(expr); ",";
    r = separated_nonempty_list(",", located_expr(expr)); ")";
    {Tuple (e::r)}
  | name = Uident; { Constr (name, None) } 
  | name = Uident; arg = located_expr(atom_expr); { Constr (name, Some arg) }
