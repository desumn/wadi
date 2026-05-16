%{
open StdLabels
open Ast
%}
%token <int> Int
%token <string> Ident

%token ParenOpen "(" ParenClose ")"

%token Plus "+" Minus "-" Star "*" Slash "/" 

%token Equal "=" 

%token Let Rec In

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
  | ~ = additive; <>

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

%inline binop_add:
  | "+"; { fun l r -> Add (l, r) }
  | "-"; { fun l r -> Sub (l, r) }
%inline binop_mul:
  | "*"; { fun l r -> Mul (l, r) }
  | "/"; { fun l r -> Div (l, r) }

let additive :=
  | l = located_expr(additive); op = binop_add; r = located_expr(multiplicative);
    { op l r }
  | ~ = multiplicative; <>

let multiplicative :=
  | l = located_expr(multiplicative); op = binop_mul; r = located_expr(app_expr);
    { op l r }
  | ~ = app_expr; <>

let app_expr :=
  | f = located_expr(app_expr); arg = located_expr(atom_expr); <App>
  | ~ = atom_expr; <>

let atom_expr :=
  | name = Ident; <Var>
  | num = Int; <Int>
  | "("; e = expr; ")"; <>
