%{
open Ast
%}
%token <int> Int
%token <string> Ident

%token ParenOpen "(" ParenClose ")"

%token Plus "+" Minus "-" Star "*" Slash "/" 

%token Equal "=" 

%token Let In

%token Eof

%nonassoc LetBody
%left Plus Minus
%left Star Slash

%start <expr> program

%%

let located_expr(X) ==
  | x = X;  { { desc = x; loc = Location.make $loc } }

let program :=
  | ~ = located_expr(expr); Eof; <>

let expr :=
  | name = Ident; <Var>
  | num = Int; <Int>
  | left = located_expr(expr); "+"; right = located_expr(expr); <Add>
  | left = located_expr(expr); "-"; right = located_expr(expr); <Sub>
  | left = located_expr(expr); "*"; right = located_expr(expr); <Mul>
  | left = located_expr(expr); "/"; right = located_expr(expr); <Div>
  | Let; name = Ident; "="; expr = located_expr(expr);
      In; body = located_expr(expr); %prec LetBody <Let>
  | "("; expr = expr; ")"; <>

