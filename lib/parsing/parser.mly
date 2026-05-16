%{
open StdLabels
open Ast

let located_bool b loc = { desc = Bool b; loc = Location.make loc }


%}
%token <int> Int
%token <string> Ident
%token <bool> Bool

%token ParenOpen "(" ParenClose ")"

%token Plus "+" Minus "-" Star "*" Slash "/" 

%token Equal "=" 

%token NotEqual "<>" Less "<" LessEqual "<=" Greater ">" GreaterEqual ">="

%token And Or

%token Let Rec In

%token If Then Else

%token Fun Arrow "->"

%token Eof

%start <expr> program

%%

let located_expr(X) ==
  | x = X;  { { desc = x; loc = Location.make $loc } }

let program :=
  | ~ = located_expr(expr); Eof; <>

let expr :=
  | ~ = open_expr; <>
  | ~ = logical_or; <>

let open_expr :=
  | Let; is_rec = boption(Rec); name = Ident; args = Ident*; "=";
    value = located_expr(expr); In; body = located_expr(expr);
    {
      let value = List.fold_right args ~init:value
        ~f:(fun arg value -> {value with desc = Lambda(arg, value)})
      in
      if is_rec then 
      LetRec (name, value, body)
      else
      Let (name, value, body)
    }
  | Fun; param = Ident; "->"; body = located_expr(expr); <Lambda>
  | If; cond = located_expr(expr); Then; then_ = located_expr(expr); Else; else_ = located_expr(expr); <If>

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
  | name = Ident; <Var>
  | num = Int; <Int>
  | bool = Bool; <Bool>
  | "("; e = expr; ")"; <>
