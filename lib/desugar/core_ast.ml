module Location = Parsing.Location
open Parsing
open StdLabels

type tuple_lit = { fst : expr; snd : expr; extra : expr list }
and constr_lit = { name : string; args : expr list }

and literal =
  | VarLit of string
  | IntLit of int
  | BoolLit of bool
  | UnitLit
  | TupleLit of tuple_lit
  | ConstrLit of constr_lit

and if_expr = { cond : expr; then_ : expr; else_ : expr }
and match_expr = { scrutinee : expr; cases : (Ast.pat * expr) list }
and lambda = { param : Ast.pat; body : expr }
and app = { left : expr; right : expr }
and let_destruct = { pattern : Ast.pat; value : expr }
and let_bind = { name : string; is_rec : bool; value : expr }

and let_expr =
  | LetDestruct of { let_destruct : let_destruct; body : expr }
  | LetBind of { let_bind : let_bind; body : expr }

and expr =
  | Lit of { literal : literal; loc : Location.t }
  | If of { if_expr : if_expr; loc : Location.t }
  | Match of { match_expr : match_expr; loc : Location.t }
  | Lambda of { lambda : lambda; loc : Location.t }
  | App of { app : app; loc : Location.t }
  | Let of { let_expr : let_expr; loc : Location.t }

and let_top = LetTopDestruct of let_destruct | LetTopBind of let_bind
and top = ExprTop of expr | LetTop of { let_top : let_top; loc : Location.t }
and program = top list

let rec desugar (top : Ast.top) : top =
  match top with
  | ExprTop e -> ExprTop (desugar_expr e)
  | LetTop lt -> LetTop { let_top = desugar_let_top lt.let_top; loc = lt.loc }

and desugar_let_top (let_top : Ast.let_top) =
  match let_top with
  | LetTopDestruct ld -> LetTopDestruct (desugar_let_destruct ld)
  | LetTopFun lf -> LetTopBind (desugar_let_fun lf)

and desugar_expr (expr : Ast.expr) =
  match expr with
  | Lit { literal; loc } -> Lit { literal = desugar_literal literal; loc }
  | UnaryOperation { operation; loc } -> desugar_unary_op operation loc
  | BinaryOperation { operation; loc } -> desugar_binary_op operation loc
  | If { if_expr; loc } -> If { if_expr = desugar_if_expr if_expr; loc }
  | Match { match_expr; loc } ->
      Match { match_expr = desugar_match_expr match_expr; loc }
  | Lambda { lambda; loc } -> Lambda { lambda = desugar_lambda lambda; loc }
  | App { app; loc } -> App { app = desugar_app app; loc }
  | Let { let_expr; loc } -> Let { let_expr = desugar_let_expr let_expr; loc }

and desugar_let_expr (let_expr : Ast.let_expr) =
  match let_expr with
  | LetDestruct { let_destruct; body } ->
      LetDestruct
        {
          let_destruct = desugar_let_destruct let_destruct;
          body = desugar_expr body;
        }
  | LetFun { let_fun; body } ->
      LetBind { let_bind = desugar_let_fun let_fun; body = desugar_expr body }

and desugar_let_destruct { pattern; value } =
  { pattern; value = desugar_expr value }

and desugar_let_fun { name; is_rec; params; value } =
  {
    name;
    is_rec;
    value =
      (if not (params = []) then
         desugar_expr
           (Lambda
              {
                loc = Location.set_ghost @@ Ast.loc_of_expr value;
                lambda = { params; body = value };
              })
       else desugar_expr value);
  }

and desugar_app { left; right } =
  { left = desugar_expr left; right = desugar_expr right }

and desugar_lambda { params; body } =
  let params = List.rev params in
  List.fold_left (List.tl params)
    ~init:{ param = List.hd params; body = desugar_expr body }
    ~f:(fun acc_lambda param ->
      { param; body = Lambda { loc = Location.dummy; lambda = acc_lambda } })

and desugar_match_expr { scrutinee; cases } =
  {
    scrutinee = desugar_expr scrutinee;
    cases = List.map cases ~f:(fun (pat, body) -> (pat, desugar_expr body));
  }

and desugar_if_expr { cond; then_; else_ } =
  {
    cond = desugar_expr cond;
    then_ = desugar_expr then_;
    else_ = desugar_expr else_;
  }

and desugar_unary_op { operator; operand } loc =
  let op_name = map_unary_op operator in
  App
    {
      loc;
      app =
        {
          left = Lit { loc = Location.dummy; literal = VarLit op_name };
          right = desugar_expr operand;
        };
    }

and desugar_binary_op { operator; left; right } loc =
  match operator with
  | And ->
      If
        {
          loc;
          if_expr =
            {
              cond = desugar_expr left;
              then_ = desugar_expr right;
              else_ = Lit { loc = Location.dummy; literal = BoolLit false };
            };
        }
  | Or ->
      If
        {
          loc;
          if_expr =
            {
              cond = desugar_expr left;
              then_ = Lit { loc = Location.dummy; literal = BoolLit true };
              else_ = desugar_expr right;
            };
        }
  | _ ->
      let op_name = map_binary_op operator in
      let inner =
        App
          {
            loc = Location.dummy;
            app =
              {
                left = Lit { loc = Location.dummy; literal = VarLit op_name };
                right = desugar_expr left;
              };
          }
      in
      App { loc; app = { left = inner; right = desugar_expr right } }

and map_unary_op op = match op with Neg -> "neg"

and map_binary_op op =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Eq -> "eq"
  | Neq -> "neq"
  | Lt -> "lt"
  | Le -> "le"
  | Gt -> "gt"
  | Ge -> "ge"
  | And | Or -> assert false

and desugar_literal lit =
  match lit with
  | VarLit v -> VarLit v
  | IntLit i -> IntLit i
  | BoolLit b -> BoolLit b
  | UnitLit -> UnitLit
  | TupleLit { fst; snd; extra } ->
      TupleLit
        {
          fst = desugar_expr fst;
          snd = desugar_expr snd;
          extra = List.map extra ~f:desugar_expr;
        }
  | ConstrLit { name; args } ->
      ConstrLit { name; args = List.map args ~f:desugar_expr }
