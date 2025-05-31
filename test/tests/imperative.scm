; Imperative code!
(define one
    (let x 10
        (begin
            (set! x (+ x 1))
            (set! x (* x 2))
            (set! x (* x x))
            x)))
(assert (= one 484))

; "Mutual Recursion"
(define even
    (let even 0
    (let odd 0
    (begin
        (set! even (lambda x (if (= x 0) #t (odd (- x 1)))))
        (set! odd (lambda x (if (= x 0) #f (even (- x 1)))))
    even))))

(assert (even 10))

; While loops
(assert
  (let x 10
    (begin
      (while (> x 5)
        (set! x (- x 1)))
      (= x 5))))


(define (fact x)
    (let n 1
    (begin
        (while (> x 0)
            (set! n (* n x))
            (set! x (- x 1)))
        n)))
(assert (= (fact 10) 3628800))


(define (log b n)
    (let c 0
    (begin
        (while (> n 1)
            (set! n (/ n b))
            (set! c (+ c 1)))
        c)))
(assert (= (log 3 59049) 10))


(define (fib n)
    (let* a 1 b 1 swp 1
    (begin
        (while (> n 0)
            (set! swp b)
            (set! b (+ a b))
            (set! a swp)
            (set! n (- n 1)))
        a)))
(assert (= (fib 10) 89))


(define (even x) (= (* (/ x 2) 2) x))
(define (collatz n)
        (let c 0
        (begin
            (while (!= n 1)
                (if (even n)
                    (set! n (/ n 2))
                    (set! n (+ (* 3 n) 1)))
                (set! c (+ c 1)))
            c)))
(assert (= (collatz 19) 20))
