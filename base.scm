(define (max m n) (if (> m n) m n))
(define (min m n) (if (< m n) m n))

(define (mod m n) (- m (* n (/ m n))))

(define (gcd m n) (if (= n 0) m (gcd n (mod m n))))

(define (lcm m n) (* m (/ n (gcd m n))))

(define (take n xs)
  (if (= n 0)
    nil
    (cons (car xs) (take (- n 1) (cdr xs)))))

(define (zipwith f xs ys)
  (cons (f (car xs) (car ys))
    (zipwith f (cdr xs) (cdr ys))))

(define (map f xs)
  (if (atom xs) nil
    (cons (f (car xs)) (map f (cdr xs)))))

(define (filter f xs)
  (if (atom xs) nil
    (if (f (car xs))
      (cons (car xs) (filter f (cdr xs)))
      (filter f (cdr xs)))))

(define (fold f init xs)
  (if (atom xs) init
    (f (car xs) (fold f init (cdr xs)))))

(define (length xs) (fold (lambda (x acc) (+ acc 1)) 0 xs))
