#lang racket
(require racket/trace)
(define (helper-first-el x lst)
  (define (helper sum xs)
    (if (null? xs)
        sum
        (helper (+ sum ((first xs) x)) (rest xs))
        )
    )
  (helper 0 lst))
 
(define (apply-end-sum lst pred-lst)
  (define (helper xs new-xs)
    (if (null? xs)
        (filter odd? new-xs)
        (helper (rest xs) (append new-xs (list (helper-first-el (first xs) pred-lst))))
        )
    )
  (helper lst '()))

(apply-end-sum '(1 2 3 4 5 6 7 8) (list (λ (x) (+ x 1)) (λ (x) (+ x 2)) identity))

(define (apply-end-sum-test lst pred-lst)
  (filter odd? (map (λ (x) (apply + (map (λ (f) (f x)) pred-lst))) lst))
  )

(apply-end-sum-test '(1 2 3 4 5 6 7 8) (list (λ (x) (+ x 1)) (λ (x) (+ x 2)) identity))