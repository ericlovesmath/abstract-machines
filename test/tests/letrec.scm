(assert (= (letrec (f x) 10 (f 5)) 10) "letrec 1")

(assert (= (letrec (f x) (f x) 10) 10) "letrec 2")

(define (fib a b n)
  (if (= n 0)
    a
    (fib b (+ a b) (- n 1))))
(assert (= (fib 1 1 10) 89) "letrec fib")

(define (fib' x)
  (if (< x 2)
    1
    (+ (fib' (- x 1)) (fib' (- x 2)))))
(assert (= (fib' 10) 89) "letrec 3")

(assert
  (letrec (factorial x)
    (if (< x 1)
      1
      (* (factorial (- x 1)) x))
    (= (factorial 10) 3628800))
  "letrec 4")

(define (flood xs)
 (if (= xs nil)
   nil
   (cons 0 (flood (cdr xs)))))
(assert (= (flood [1 2 3 4]) [0 0 0 0]) "letrec 5")

(define (length xs)
 (if (= xs nil)
   0
   (+ 1 (length (cdr xs)))))
(assert (= (length [1 2 3 4]) 4) "letrec 6")
