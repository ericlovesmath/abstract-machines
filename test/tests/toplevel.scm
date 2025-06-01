; Test [use] form
(use "tests/use.scm")
(assert (= (decr (decr (incr 5))) 4) "toplevel use")

; Variable Define
(define x 10)
(assert (= (* x 2) 20) "toplevel 1")

; Function Define
(define (f x) (* x (- x 1)))
(assert (= (f 10) 90) "toplevel 2")

; Capturing [x]
(define (f y) (+ y x))
(assert (= (f 30) 40) "toplevel 3")

; Test enclosed [x] in [f]
(define x 1)
(assert (= x 1) "toplevel 4")
(assert (= (f 30) 40) "toplevel 5")
