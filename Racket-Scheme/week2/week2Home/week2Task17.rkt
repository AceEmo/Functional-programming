#lang racket
#|
Description:
Define a procedure that removes the first occurrence of a digit in a number by going from right to left.
|#
(require racket/trace)

(define (remove-first-occurrence num digit)
  (define (helper num digit found)
    (cond
      [(zero? num) 0]
      [(and (= (remainder num 10) digit) (not found)) 
       (helper (quotient num 10) digit #t)] ; Skip the first occurrence of the digit
      [else
       (+ (* (helper (quotient num 10) digit found) 10) (remainder num 10))])) ; Keep the digit
  (trace helper)
  (helper num digit #f))

(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 11 1) 1)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)