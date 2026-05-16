open StdLabels
open Alcotest
open Wadi

let value = testable Eval.pp_value Eval.equal_value
let eval_error = testable Eval.pp_error Eval.equal_error

let eval_test name ast expected =
  check' (result value eval_error) ~msg:name ~expected
    ~actual:(Eval.eval_expr Eval.Env.empty ast)

let eval_case name ast expected =
  test_case name `Quick @@ fun () -> eval_test name ast expected

let parse = Parsing.parse_string ~name:"test"

let eval_cases =
  [
    eval_case "simple" (parse "2 + 10") (Ok (Int 12));
    eval_case "composite" (parse "2 * 10 - 30 + 90 * 4") (Ok (Int 350));
    eval_case "let-binding"
      (parse "let x = 39 in let y = 11 in y + x * y")
      (Ok (Int 440));
    eval_case "unbound variable"
      (parse "let x = 34 in x * z")
      (Error (Unbound_variable "z"));
    eval_case "divide by zero"
      (parse "let x = 10 in let a = 32 in x * a / 0")
      (Error Divide_by_zero);
    eval_case "with_comments"
      (parse "(* add two numbers *) 30 + 40")
      (Ok (Int 70));
    eval_case "lambda apply" (parse "(fun x -> x + 1) 5") (Ok (Int 6));
    eval_case "let lambda"
      (parse "let f = fun x -> x * 2 in f 21")
      (Ok (Int 42));
    eval_case "curried"
      (parse "let add = fun x -> fun y -> x + y in add 3 4")
      (Ok (Int 7));
    eval_case "curried sugar"
      (parse "let add x y = x + y in add 3 4")
      (Ok (Int 7));
    eval_case "partial app"
      (parse "let add x y = x + y in let inc = add 1 in inc 10")
      (Ok (Int 11));
    eval_case "lexical capture"
      (parse "let x = 1 in let f = fun y -> x + y in let x = 99 in f 0")
      (Ok (Int 1));
  ]
