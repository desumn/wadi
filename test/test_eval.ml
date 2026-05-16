open StdLabels
open Alcotest
open Wadi

let eval_error = testable Eval.pp_error Eval.equal_error

let eval_test name ast expected =
  check' (result int eval_error) ~msg:name ~expected
    ~actual:(Eval.eval_expr Eval.Env.empty ast)

let eval_case name ast expected =
  test_case name `Quick @@ fun () -> eval_test name ast expected

let parse = Parsing.parse_string ~name:"test"

let eval_cases =
  [
    eval_case "simple" (parse "2 + 10") (Ok 12);
    eval_case "composite" (parse "2 * 10 - 30 + 90 * 4") (Ok 350);
    eval_case "let-binding"
      (parse "let x = 39 in let y = 11 in y + x * y")
      (Ok 440);
    eval_case "unbound variable"
      (parse "let x = 34 in x * z")
      (Error (Unbound_variable "z"));
    eval_case "divide by zero"
      (parse "let x = 10 in let a = 32 in x * a / 0")
      (Error Divide_by_zero);
  ]
