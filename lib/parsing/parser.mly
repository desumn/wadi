%{
open Ast

let mk_loc loc = Location.make loc

let lit loc literal =
  Lit { literal; loc = mk_loc loc }

let unary_op loc operator operand =
  UnaryOperation {
    operation = { operator; operand };
    loc = mk_loc loc;
  }

let binary_op loc operator left right =
  BinaryOperation {
    operation = { operator; left; right };
    loc = mk_loc loc;
  }

let if_ loc cond then_ else_ =
  If {
    if_expr = { cond; then_; else_ };
    loc = mk_loc loc;
  }

let match_ loc scrutinee cases =
  Match {
    match_expr = { scrutinee; cases };
    loc = mk_loc loc;
  }

let lambda_ loc params body =
  Lambda {
    lambda = { params; body };
    loc = mk_loc loc;
  }

let app_ loc left right =
  App {
    app = { left; right };
    loc = mk_loc loc;
  }

let let_ loc let_expr =
  Let { let_expr; loc = mk_loc loc }


let pat_ loc structural_pat =
  Structural { structural_pat; loc = mk_loc loc }


let top_expr loc expr =
  ExprTop expr
%}


%token <int> Int
%token <bool> Bool
%token <string> Lident
%token <string> Uident

%token ParenOpen "(" ParenClose ")"
%token Comma ","
%token Bar "|"
%token Underscore "_"
%token Arrow "->"
%token Equal "="

%token Plus "+" Minus "-" Star "*" Slash "/"

%token NotEqual "<>" Less "<" LessEqual "<=" Greater ">" GreaterEqual ">="

%token And Or

%token Pipe "|>" RevPipe "<|"

%token Let Rec In
%token If Then Else
%token Fun
%token Match With End

%token Eof

%start <program> program

%%

let program :=
  | items = list(top_item); Eof; { items }

let top_item :=
  | Let; b = let_destruct_binding;
    { LetTop { let_top = (LetTopDestruct b); loc = Location.make $loc } }
  | Let; b = let_fun_binding;
    { LetTop { let_top = (LetTopFun b); loc = Location.make $loc }}
  | e = expr;
    { top_expr $loc e }

let let_destruct_binding :=
  | pat = pat; "="; value = expr;
    { { pattern=pat; value } }

let let_fun_binding :=
  | Rec; name = Lident; params = list(atom_pat); "="; value = expr;
    { { name; is_rec = true; params; value } }
  | name = Lident; arg = atom_pat; params = list(atom_pat); "="; value = expr;
    { { name; is_rec = false; params = arg :: params; value } }

let expr :=
  | ~ = open_expr; <>
  | ~ = pipe_expr; <>

let open_expr :=
  | Let; b = let_destruct_binding; In; body = expr;
    { let_ $loc (LetDestruct { let_destruct = b; body }) }
  | Let; b = let_fun_binding; In; body = expr;
    { let_ $loc (LetFun { let_fun = b; body }) }
  | Fun; params = nonempty_list(atom_pat); "->"; body = expr;
    { lambda_ $loc params body }
  | If; cond = expr; Then; then_ = expr; Else; else_ = expr;
    { if_ $loc cond then_ else_ }
  | Match; e = expr; With; cases = match_cases; End;
    { match_ $loc e cases }

let match_cases :=
  | "|"?; bs = separated_nonempty_list("|", match_branch); { bs }

let match_branch :=
  | p = pat; "->"; e = expr; { (p, e) }

let pipe_expr :=
  | l = pipe_expr; "|>"; r = rpipe_expr;
    { app_ $loc r l }
  | ~ = rpipe_expr; <>

let rpipe_expr :=
  | l = logical_or; "<|"; r = rpipe_expr;
    { app_ $loc l r }
  | ~ = logical_or; <>

let logical_or :=
  | l = logical_or; Or; r = logical_and;
    { binary_op $loc Or l r }
  | ~ = logical_and; <>

let logical_and :=
  | l = logical_and; And; r = comparison;
    { binary_op $loc And l r }
  | ~ = comparison; <>

let comparison :=
  | l = additive; op = comp_op; r = additive;
    { binary_op $loc op l r }
  | ~ = additive; <>

%inline comp_op:
  | "=" { Eq }
  | "<>" { Neq }
  | "<" { Lt }
  | "<=" { Le }
  | ">" { Gt }
  | ">=" { Ge }

let additive :=
  | l = additive; op = binop_add; r = multiplicative;
    { binary_op $loc op l r }
  | ~ = multiplicative; <>

%inline binop_add:
  | "+" { Add }
  | "-" { Sub }

let multiplicative :=
  | l = multiplicative; op = binop_mul; r = unary_expr;
    { binary_op $loc op l r }
  | ~ = unary_expr; <>

%inline binop_mul:
  | "*" { Mul }
  | "/" { Div }

let unary_expr :=
  | "-"; e = unary_expr;
    { unary_op $loc Neg e }
  | ~ = app_expr; <>

let app_expr :=
  | f = app_expr; arg = atom_expr;
    { app_ $loc f arg }
  | name = Uident; arg = atom_expr;
    { lit $loc (ConstrLit { name; args = [arg] }) }
  | ~ = atom_expr; <>

let atom_expr :=
  | name = Lident;
    { lit $loc (VarLit name) }
  | n = Int;
    { lit $loc (IntLit n) }
  | b = Bool;
    { lit $loc (BoolLit b) }
  | name = Uident;
    { lit $loc (ConstrLit { name; args = [] }) }
  | "("; ")";
    { lit $loc UnitLit }
  | "("; e = expr; ")";
    { e }
  | "("; e1 = expr; ","; e2 = expr;
      rest = list(","; e = expr; <>); ")";
    { lit $loc (TupleLit { fst = e1; snd = e2; extra = rest }) }

let pat :=
  | ~ = atom_pat; <>
  | "("; p1 = pat; ","; p2 = pat;
      rest = list(","; p = pat; <>); ")";
    { pat_ $loc (TuplePat { fst = p1; snd = p2; extra = rest }) }

let atom_pat :=
  | "_"; { pat_ $loc WildcardPat }
  | name = Lident;
    { pat_ $loc (VarPat name) }
  | n = Int;
    { pat_ $loc (IntPat n) }
  | b = Bool;
    { pat_ $loc (BoolPat b) }
  | name = Uident;
    { pat_ $loc (ConstrPat { name; args = [] }) }
  | name = Uident; arg = atom_pat;
    { pat_ $loc (ConstrPat { name; args = [arg] }) }
  | "("; ")";
    { pat_ $loc UnitPat }
  | "("; p = pat; ")";
    { p }
