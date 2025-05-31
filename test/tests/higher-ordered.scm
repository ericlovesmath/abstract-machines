; Testing basic higher ordered functions

(assert
  (let (compose f g)
    (lambda x (f (g x)))

    (let f (compose
      (lambda x (+ x 1))
      (lambda x (* x 2)))

    (= (f 10) 21))))


; Nicer let* syntax
(assert
  (let*
    (plus x y) (+ x y)
    (minus x y) (- x y)
    (dual switch) (if (= switch 0) plus minus)
  (and
    (= ((dual 0) 5 3) 8)
    (= ((dual 1) 5 3) 2))))
