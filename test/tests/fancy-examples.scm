; Y-combinator!
(assert
  (let Y
    (lambda (f)
      ((lambda x (f (lambda n ((x x) n))))
       (lambda x (f (lambda n ((x x) n))))))
  
    (let factgen
      (lambda self
        (lambda n
          (if (= n 0)
              1
              (* n (self (- n 1))))))
  
    (let factorial
      (Y factgen)
  
    (= (factorial 5) 120))))
  "fancy y-combinator factorial")


(define (map f xs)
 (if (atom xs)
   nil
   (cons (f (car xs)) (map f (cdr xs)))))

(assert (= (map (lambda x (* x 2)) [3 5 7]) [6 10 14]) "fancy map even")


; Some funny [fold] functions
(define (fold f init xs)
 (if (atom xs)
   init
   (f (car xs) (fold f init (cdr xs)))))

(define (len xs)  (fold (lambda (x acc) (+ acc 1)) 0 xs))
(define (sum xs)  (fold (lambda (x acc) (+ acc x)) 0 xs))
(define (prod xs) (fold (lambda (x acc) (* acc x)) 1 xs))

(assert
  (let* list [3 5 7 8]
  (= [(len list) (sum list) (prod list)] [4 23 840]))
  "fancy folds")
