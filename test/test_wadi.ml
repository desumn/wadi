open Alcotest


let () = Alcotest.run "wadi"
  [
    "eval", Test_eval.eval_cases;
  ]
