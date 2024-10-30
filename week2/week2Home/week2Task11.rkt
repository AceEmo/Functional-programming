#lang racket
(require racket/trace)

;; Function to check if a number is prime
(define (is-prime? n)
  (define (check-divisor? i)
    (if (> (* i i) n)
        #t
        (if (zero? (remainder n i))
            #f
            (check-divisor? (add1 i)))))
  (if (< n 2)
      #f
      (check-divisor? 2)))

;; Helper function to calculate cubic prime
(define (cubic-prime n)
  (- (expt n 3) (expt (sub1 n) 3))) ;; Computes n^3 - (n-1)^3

;; Function to find the nth cubic prime number
(define (nth-cubic n)
  (define (helper current counter last-prime)
    (if (eq? counter n)
        last-prime
        (if (is-prime? (cubic-prime current))
            (helper (add1 current) (add1 counter) (cubic-prime current))
            (helper (add1 current) counter last-prime))))
  ;(trace helper)
  (helper 1 0 0))

(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61) ; 61 is the 4th cubic prime number
(= (nth-cubic 50) 55897) ; 55897 is the 50th cubic prime number
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)