#lang racket
(require math/number-theory)
(require racket/trace)

;TASK1
(define (primes-prod x)
  (define (helper prod current end)
    (if (> current end)
        prod
        (if (prime? current)
            (helper (* prod current) (add1 current) end)
            (helper prod (add1 current) end)
            )
        )
    )
  (helper 1 2 (sqrt x)))

(equal? (primes-prod 12) 6)
(equal? (primes-prod 49) 210)
(equal? (primes-prod 1200) 200560490130)

;TASK2
(define (shuffle-merge lst1 lst2)
  (define (helper new-xs xs1 xs2 counter)
    (if (null? xs1)
        (append new-xs xs2)
        (if (null? xs2)
            (append new-xs xs1)
            (if (even? counter)
                (helper (append new-xs (list (first xs1))) (rest xs1) xs2 (add1 counter))
                (helper (append new-xs (list (first xs2))) xs1 (rest xs2) (add1 counter))
                )
            )
        )
    )
  (helper '() lst1 lst2 0))

(equal? (shuffle-merge '(1) '()) '(1))
(equal? (shuffle-merge '(3 4 5) '(2)) '(3 2 4 5))
(equal? (shuffle-merge '(3 4 5) '(9 2)) '(3 9 4 2 5))
(equal? (shuffle-merge '(3 2 8) '(5 6 1 9 11)) '(3 5 2 6 8 1 9 11))

;TASK3
(define (g-l-sum limit)
  (define (helper x y)
    (if (eq? (+ (gcd x y) (lcm x y)) limit)
        (cons x y)
        (if (< (gcd x y) (lcm x y))
            (helper (add1 x) y)
            (helper x (add1 y))
            )
        )
    )
  (helper 1 1))

(equal? (g-l-sum 2) '(1 . 1))
(equal? (g-l-sum 14) '(7 . 7))

;TASK4
(define (find-pos-x xss)
  (define (helper xss row)
    (if (list? (member 1 (first xss)))
        row
        (helper (rest xss) (add1 row))
        )
    )
  (helper xss 1))

(define (find-pos-y xss)
  (define (helper xss xs pos)
        (if (null? xs)
            (helper (rest xss) (first xss) 1)
            (if (eq? (first xs) 1)
                pos
                (helper xss (rest xs) (add1 pos))
                )
            )
    )
  (helper xss (first xss) 1))

(define (steps-bm xss)
  (+ (abs (- (find-pos-x xss) 3)) (abs (- (find-pos-y xss) 3)))
  )

(equal? (steps-bm '((0 0 0 0 0)
                    (0 0 0 0 1)
                    (0 0 0 0 0)
                    (0 0 0 0 0)
                    (0 0 0 0 0))) 3)

(equal? (steps-bm '((0 0 0 0 0)
                    (0 0 0 0 0)
                    (0 1 0 0 0)
                    (0 0 0 0 0)
                    (0 0 0 0 0))) 1)

(equal? (steps-bm '((0 0 0 0 0)
                    (0 0 0 0 0)
                    (0 0 1 0 0)
                    (0 0 0 0 0)
                    (0 0 0 0 0))) 0)

(equal? (steps-bm '((0 0 0 0 0)
                    (0 0 0 0 0)
                    (0 0 0 0 0)
                    (0 0 0 0 0)
                    (0 0 0 0 1))) 4)
