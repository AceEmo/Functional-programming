#lang racket
#|
Description:
Define a predicate that accepts a natural number n and returns whether n^2 ends in the digits of n.
|#

(require racket/trace)

(define (ends-in-digits-of? a b)
  (if (zero? b)
      #t
      (if (eq? (remainder a 10) (remainder b 10))
          (ends-in-digits-of? (quotient a 10) (quotient b 10))
          #f
          )
      )
  )

(define (automorphic? n)
  (ends-in-digits-of? (* n n) n)
  )

(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)