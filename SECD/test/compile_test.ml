open Lib

let _ =
  let ( == ) program expected =
    assert (Compile.execute program = expected);
  in
  "(+ 1 2)" == Int 3;
  "(cons 1 nil)" == List [Int 1];
  "(* (/ (+ 5 7) 3) (- 14 1))" == Int 52;
  "(cdr (cons (car [2 3 4]) nil))" == List []

let _ =
  let fail program =
    match Compile.execute program with
    | exception _ -> ()
    | _ -> failwith "Expected Failure"
  in
  fail "(+ 1)";
  fail "(+ 1)";
