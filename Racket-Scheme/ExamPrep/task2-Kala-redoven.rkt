#lang racket
(require racket/trace)
(define (num-to-xs n)
  (define (helper num xs)
    (if (zero? num)
        xs
        (helper (quotient num 10) (cons (remainder num 10) xs))
        )
    )
  (helper n '()))

(define (xs-to-num xs)
  (define (helper lst num-str)
    (if (null? lst)
        (string->number num-str)
        (helper (rest lst) (string-append num-str (number->string (first lst))))))
  (helper xs ""))

(define (transform xs)
  (define (helper new-xs lst)
    (if (null? lst)
        new-xs
        (if (eq? (first lst) 1)
            (helper (append new-xs (list 7)) (rest lst))
            (if (eq? (first lst) 7)
                (helper (append new-xs (list 117)) (rest lst))
                 #f
                 )
            )
        )
    ) ;(trace helper)
  (helper '() xs))

(define (sequence i)
  (define (helper-seq current index)
    (if (eq? index 1)
        current
        (helper-seq (xs-to-num (transform (num-to-xs current))) (sub1 index))
        )
    )
  (helper-seq 1 i))

(define (digit j i)
  (define (helper index xs)
    (list-ref xs (sub1 index))
    ) 
  (helper j (num-to-xs (sequence i))))
(digit 1 2) ;→ 7 
(digit 5 4)  ;→ 7 
(digit 6 5)  ;→ 1 
(digit 7 10) ;→ 1 
(digit 9 10) ;→ 1 
(digit 9 12) ;→ 1 
