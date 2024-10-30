#lang racket
#|
Description:
Define a procedure that takes a single argument procedure and returns it applied n times.
|#

(define (apply-n f n)
  (if (zero? n)
      (λ (x) x) ; return the identity function when n is 0
      (λ (x) ((apply-n f (sub1 n)) (f x))) ; apply f and recursively call apply-n
      )
  )

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)
