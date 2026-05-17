%{
open StdLabels
open Ast


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
%token End

%token If Then Else

%token Fun Arrow "->"

%token Pipe "|>" RevPipe "<|"

%token Eof

%start <program> program

%%

let program :=
  | Eof; {[ExprTop (Lit { literal = UnitLit; loc = Location.make $loc })]}
