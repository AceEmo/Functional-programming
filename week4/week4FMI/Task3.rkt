#lang racket

(define (elem-rec-with-mc? x xs)
  (and (not (null? xs)) (or (eq? x (car xs))))
  )

(define (elem-proc? x xs)
  (list? (member x xs))
  )

(equal? (elem-rec-with-mc? 1 '(5 2 1)) #t)
(equal? (elem-rec-with-mc? "str" '()) #f)
(equal? (elem-rec-with-mc? "str" '("str" "len" "pair")) #t)