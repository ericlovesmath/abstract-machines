; Variable Define
(define x 10)
(assert (= (* x 2) 20))

; Function Define
(define (f x) (* x (- x 1)))
(assert (= (f 10) 90))

; Capturing [x]
(define (f y) (+ y x))
(assert (= (f 30) 40))

; Test enclosed [x] in [f]
(define x 1)
(assert (= x 1))
(assert (= (f 30) 40))
