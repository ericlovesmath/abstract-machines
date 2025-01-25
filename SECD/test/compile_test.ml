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
  "(if (if (= 5 3) (< 1 2) (< 2 1)) 7 8)" == Int 8;
  "((lambda x (+ x 1)) 10)" == Int 11;
  "(let x (+ (let x 3 (+ x x)) 7) x)" == Int 13;

  (* Let binded functions *)
  "(let (add x y)
        (+ x y)
        (add 7 3))"
  == Int 10;

  (* Y Combinator, factorial *)
  "(let Y
  (lambda (f)
    ((lambda x (f (lambda n ((x x) n))))
     (lambda x (f (lambda n ((x x) n))))))

  (let factgen
    (lambda self
      (lambda n
        (if (= n 0)
            1
            (* n (self (- n 1))))))

  (let factorial
    (Y factgen)

  (factorial 5))))"
  == Int 120;

  (* Higher ordered functions, compose *)
  "(let (compose f g)
    (lambda x (f (g x)))

  (let f (compose
    (lambda x (+ x 1))
    (lambda x (* x 2)))

  (f 10)))"
  == Int 21

  (* "(letrec (map f xs) *)
  (*   (if (= xs nil) *)
  (*     nil *)
  (*     (cons (f (car xs)) (map f (cdr xs)))) *)
  (**)
  (* (let (double x) (* x 2) *)
  (**)
  (* (map double [3 5 7])))" *)
  (* == List [Int 6; Int 10; Int 14] *)


let _ =
  let fail program =
    match Compile.execute program with
    | exception _ -> ()
    | _ -> failwith "Expected Failure"
  in
  fail "(+ 1)";
