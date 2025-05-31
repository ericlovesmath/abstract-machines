; call/cc, let/cc, and simulation control flow!
(assert (= (callcc (lambda k (k 42))) 42))

(assert (= (+ 1 (callcc (lambda k (+ 2 (k 3))))) 4))

(assert (= (+ 1 (letcc k (+ 2 (k 3)))) 4))

; Simulating return
(define (f x)
    (letcc return (begin
        (if (< x 5)
            (return (+ 1 x)))
        (f (- x 1)))))
(assert (= (f 10) 5))

(define (fib x)
    (letcc return
        (let n 0
        (begin
            (while #t
                (if (= x 0)
                    (return n))
                (set! n (+ n x))
                (set! x (- x 1)))
            n))))
(assert (= (fib 10) 55))

; Simulating continue / break
(define (even n) (= (* 2 (/ n 2)) n))
(assert
  (let c 1
  (begin
    (let i 0
    (let continue #u
    (while (< i 10)
      (letcc k (begin
        (set! continue k)
        (set! i (+ i 1))
        (if (even i)
            continue
            (set! c (* c i))))))))
    (= c 945))))

; Monadic Delimited Continuations!!
(assert (= 3
  (let*
      metak #u
  
      (reset thunk)
        (let oldmetak metak
          (letcc k (begin
            (set! metak
              (lambda v (begin
                (set! metak oldmetak)
                (k v))))
            (metak (thunk #u)))))
  
      (shift f)
        (letcc k
           (let (capturedk v) (reset (lambda n (k v)))
              (metak (f capturedk))))
  
  (reset (lambda n
    (+ 1 (shift (lambda k
      (k 2)))))))))

; Random example of throw
(define (safediv x y)
    (letcc throw (begin
        (if (= y 0)
            (throw -1))
        (/ x y))))
(assert (and (= (safediv 10 0) -1) (= (safediv 10 5) 2)))
