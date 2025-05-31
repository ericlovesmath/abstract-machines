(define (take n xs)
  (if (= n 0)
    nil
    (cons (car xs) (take (- n 1) (cdr xs)))))

; Super cool lazy function!
(assert (=
  (letrec (fib a b)
    (cons a (fib b (+ a b)))
  (take 10 (fib 1 1)))
  [1 1 2 3 5 8 13 21 34 55]))


(define (zipwith f xs ys)
  (cons (f (car xs) (car ys))
    (zipwith f (cdr xs) (cdr ys))))

; Super cool infinite lists!
(assert (=
  (letrec fibs (cons 1 (cons 1 (zipwith + fibs (cdr fibs))))
    (take 10 fibs))
  [1 1 2 3 5 8 13 21 34 55]))

(define (filter f xs)
  (cond
    ((atom xs) nil)
    ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
    (#t (filter f (cdr xs)))))

(define (even x) (= (* 2 (/ x 2)) x))

(assert (=
  (letrec (natsfrom n) (cons n (natsfrom (+ n 1)))
    (take 5 (filter even (natsfrom 0))))
  [0 2 4 6 8]))

(define (map f xs)
  (if (atom xs)
    nil
    (cons (f (car xs)) (map f (cdr xs)))))

(assert (=
  (letrec nats (cons 0 (map (+ 1) nats))
    (take 5 (map (* 2) nats)))   ; Wow we have currying too
  [0 2 4 6 8]))
