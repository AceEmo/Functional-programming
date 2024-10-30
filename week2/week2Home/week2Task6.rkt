#lang racket
#|

Description:
Define a procedure sum-special-primes n d that returns the sum of the first n prime numbers that contain the digit d.

|#
(require math/number-theory)
(require racket/trace)

(define (is-prime? n)
  (define (helper d)
    (cond
      [(>= d n) #t]
      [(divides? d n) #f]
      [else (helper (add1 d))]
      )
    )
  (if (negative? n)
      (error "n has to be non-negative")
      (and (> n 1) (helper 2))
      )
  )

(define (contains-digit? n d)
  (if (zero? n)
      #f
      (if (eq? (remainder n 10) d)
          #t
          (contains-digit? (quotient n 10) d)
          )
      )
  )


(define (sum-special-primes n d)
  (define (helper current check sum)
    (if (eq? current n)
        sum
        (if (and (is-prime? check) (contains-digit? check d))
            (helper (add1 current) (add1 check) (+ sum check))
            (helper current (add1 check) sum)
            )
        )
    )
  ;(trace helper)
  (helper 0 2 0))



(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)