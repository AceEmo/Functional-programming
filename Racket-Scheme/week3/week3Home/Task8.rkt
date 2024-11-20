#lang racket
#|
Description:

Using accumulate define a procedure that:
- checks whether a number in an interval passes a predicate p?;
- for a given n (assume n > 2) calculates the expression 2^3 + 5^3 + 8^3 + ... + n^3;
- calculates the factorial of a number;
- checks whether a number is prime.
|#
(require racket/trace)

(define (accumulate f acc start end transform next)
  (if (> start end)
      acc
      (accumulate f (f (transform start) acc) (next start) end transform next)
      )
  )

(define (any? start end p?)
  (accumulate (λ (x result) (or result (p? x))) #f start end identity add1))

(equal? (any? 1001 1500 (λ (x) (< x 1000))) #f)
(equal? (any? 1 100 odd?) #t)

(define (cubed-interval-till n)
  (accumulate + 0 2 n (λ (x) (expt x 3)) (curry + 3))
  )

(= (cubed-interval-till 11) 1976)
(= (cubed-interval-till 15) 4720)

(define (fact-accum n)
  (accumulate * 1 1 n identity add1)
  )

(= (fact-accum 5) 120)
(= (fact-accum 8) 40320)

(define (prime-accum? n)
  (accumulate (λ (divisor result) (and result (not (zero? (remainder n divisor))))) #t 2 (sub1 n) identity add1)
  )

(equal? (prime-accum? 1) #f)
(equal? (prime-accum? 2) #t)
(equal? (prime-accum? 3) #t)
(equal? (prime-accum? 6) #f)
(equal? (prime-accum? 42) #f)
(equal? (prime-accum? 61) #t)
