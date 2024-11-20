#lang racket
#|
Description:
Define the procedure derive-n f n eps that returns the n^th order derivative of an unary procedure f with precision eps.
|#

(define (derive f eps)
  (λ (x) (/ (- (f (+ x eps)) (f x)) eps)))

(define (derive-n f n eps)
  (if (zero? n)
      f
      (derive (derive-n f (sub1 n) eps) eps))) 

(= ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2) 12.000015203739167)