#lang racket
#|
Description:
Define a procedure that returns the number of occurrences of a digit in a non-negative number.
|#

(define (count-occurrences n d)
  (if (zero? n)
      0
      (if (eq? (remainder n 10) d)
          (add1 (count-occurrences (quotient n 10) d))
          (count-occurrences (quotient n 10) d)
          )
      )
  )

(= (count-occurrences 121 1) 2)
(= (count-occurrences 222 1) 0)
(= (count-occurrences 100 0) 2)