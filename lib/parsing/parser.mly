%{
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

%nonassoc App_
%nonassoc LetBody
%left Plus Minus
%left Star Slash

%start <expr> program

%%

let option_bool(X) ==
  | x = X?; {match x with None -> false | Some _ -> true}

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
  | Let; is_rec = option_bool(Rec); name = Ident; "="; expr = located_expr(expr);
      In; body = located_expr(expr); %prec LetBody {Let(name, expr, body, is_rec)}
  | on = located_expr(expr); value = located_expr(expr); %prec App_ <App>
  | Fun; param = Ident; "->"; body = located_expr(expr); <Lambda>
  | "("; expr = expr; ")"; <>


