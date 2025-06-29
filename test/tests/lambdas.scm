(assert (= ((lambda x (+ x 1)) 10) 11) "lambda 1")

(assert (= (let x (+ (let x 3 (+ x x)) 7) x) 13) "lambda 2")

(assert 
  (let (add x y)
    (+ x y)
    (= (add 7 3) 10))
  "lambda 3")

(assert
  (let* (add x y) (+ x y)
        (times x y) (* x y)
  (= (times (add 7 3) (add 1 2)) 30))
  "lambda 4")
