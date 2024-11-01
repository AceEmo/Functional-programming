#lang racket
(require racket/trace)

(define (sum-digits n)
  (if (zero? n)
      0
      (+ (remainder n 10) (sum-digits (quotient n 10)))
      ))

(define (count-digits n)
  (if (< n 10)
      1
      (add1 (count-digits (quotient n 10)))
      ))

(define (digital-root-count n)
  (define (helper counter n)
    (if (< n 10)
      counter
      (helper (add1 counter) (sum-digits n))))
  (helper 0 n))

(define (min-endurance-max-length a b)
  (define (helper start min-endr-max-len len endr)
    (if (> start (max a b))
        min-endr-max-len
        (if (or
             (and (eq? (digital-root-count start) endr)
                  (> (count-digits start) len))
             (< (digital-root-count start) endr))
            (helper (add1 start) start (count-digits start) (digital-root-count start))
                (helper (add1 start) min-endr-max-len len endr))))
  (helper (min a b) a (count-digits a) (digital-root-count a)))

(equal? (min-endurance-max-length 10 1) 1)
(equal? (min-endurance-max-length 333 1000) 1000)
(equal? (min-endurance-max-length 333 2000) 1000)
(equal? (min-endurance-max-length 356 460) 360)
(equal? (min-endurance-max-length 498 701) 500)
(equal? (min-endurance-max-length 583 889) 600)
(equal? (min-endurance-max-length 34 621) 100)
(equal? (min-endurance-max-length 651 234) 234)
(equal? (min-endurance-max-length 234 651) 234)

(define (all-zero? xs) (andmap zero? xs))

(define (eval-list xs)
  (if (equal? (length xs) 1)
      '()
      (cons (- (second xs) (first xs)) (eval-list (rest xs)))
      ))

(define (helper-back xs)
  (if (all-zero? xs)
      0
      (- (first xs) (helper-back (eval-list xs)))
      ))

(define (helper-front xs)
  (if (all-zero? xs)
      0
      (+ (last xs) (helper-front (eval-list xs)))
      ))

(define (sum-predictions strategy xs)
  (if (equal? strategy "backwards")
      (apply + (map (λ (x) (helper-back x)) xs))
      (apply + (map (λ (x) (helper-front x)) xs))
      ))

(equal? (sum-predictions "backwards" (list (list 10 13 16 21 30 45))) 5)
(equal? (sum-predictions "forwards" (list (list 10 13 16 21 30 45))) 68)
(equal? (sum-predictions "backwards" (list (list 7 9 12 16 21 27 34))) 6)
(equal? (sum-predictions "forwards" (list (list 7 9 12 16 21 27 34))) 42)
(equal? (sum-predictions "forwards" (list (list 7 9 12) (list 7 9 12 16) (list 7 9 12 16 21))) 64)
(equal? (sum-predictions "backwards" (list (list 21 30 45) (list 16 21 30 45) (list 13 16 21 30 45))) 41)
(equal? (sum-predictions "forwards" (list (list 0 3 6 9 12 15) (list 1 3 6 10 15 21) (list 10 13 16 21 30 45))) 114)