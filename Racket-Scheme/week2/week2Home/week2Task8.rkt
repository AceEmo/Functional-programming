#lang racket
#|
Description:
A number is interesting if and only if it is evenly divided by the sum of its digits. Define a predicate that checks whether a number is interesting.
|#

(require racket/trace)

(define (sum-of-digits n)
  (if (zero? n)
      0
      (+ (remainder n 10) (sum-of-digits (quotient n 10)))
      )
  )

(define (interesting? n)
  (if (zero? (remainder n (sum-of-digits n)))
      #t
      #f
      )
  )
;(trace interesting?)

(equal? (interesting? 410) #t)
(equal? (interesting? 212) #f)
(equal? (interesting? 567) #f)
(equal? (interesting? 70) #t)
(equal? (interesting? 5) #t)
(equal? (interesting? 4) #t)