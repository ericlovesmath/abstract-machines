; Testing basic syntax and expressions

(assert (= (+ 1 2) 3))

(assert (= (cons 1 nil) [1]))

(assert (= (* (/ (+ 5 7) 3) (- 14 1)) 52))

(assert (= (cdr (cons (car [2 3 4]) nil)) []))

(assert (= (if (= 5 3) [5 2] (if #t 10 11)) 10))

(assert (= (if (if #f (< 1 2) (< 2 1)) 7 8) 8))

(assert (= (!= (and #t #f) (or #f #f)) #f))

(assert (=

    (+ ; Comments can be inlines
        ; Or in newlines
        1 ; And ignores all expressions after the semicolon
        2
    )

3))

; error is a special form, the string is fake and not a real value
(if #t 1 (error "hi"))

; assert can have a custom error form
(assert #t "custom error")
