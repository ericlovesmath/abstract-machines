open Lib

let _ =
  let ( == ) program expected =
    assert (Compile.execute program = expected);
  in
  "(+ 1 2)" == Int 3;
  "(cons 1 nil)" == List [Int 1];
  "(* (/ (+ 5 7) 3) (- 14 1))" == Int 52;
  "(cdr (cons (car [2 3 4]) nil))" == List [];
  "(if (= 5 3) [5 2] (if (> 5 3) 10 11))" == Int 10;
  "(if (if (= 5 3) (< 1 2) (< 2 1)) 7 8)" == Int 8

let _ =
  let fail program =
    match Compile.execute program with
    | exception _ -> ()
    | _ -> failwith "Expected Failure"
  in
  fail "(+ 1)";
  fail "(+ 1)";
