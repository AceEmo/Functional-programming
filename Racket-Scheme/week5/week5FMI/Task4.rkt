#lang racket
(require racket/trace)

(define (num-to-xs num)
  (define (helper xs n)
    (if (zero? n)
        xs
        (helper (cons (remainder n 10) xs) (quotient n 10))
        )
    )
  (helper '() num))

(define (xs-to-num lst)
  (string->number (apply string-append (map number->string lst))))

(define (sum-with-possition n)
  (define (helper xs new-xs len)
    (if (null? xs)
        (xs-to-num new-xs)
        (helper (rest xs) (append new-xs (list (+ (first xs) len))) (sub1 len))
        )
    )
  (helper (num-to-xs n) '() (length (num-to-xs n))))


(sum-with-possition 123)
(sum-with-possition 507)
(sum-with-possition 987)
(sum-with-possition 1249)
(sum-with-possition 9000)

(define (minimum pred l)
  (define (helper min-el xs)
    (if (null? xs)
        min-el
        (if (> min-el (pred (first xs)))
            (helper (first xs) (rest xs))
            (helper min-el (rest xs))
            )
        )
    )
  (helper (pred (first l)) (rest l)))

(minimum (λ (x) (+ x 1)) '(2 3 4))
(minimum (λ (x) (* x 2)) '(2 3 4))
(minimum (λ (x) (expt x 3)) '(2 3 4))
(minimum (λ (x) (+ x 2)) '(2 3 4))
(minimum (λ (x) (sqrt x)) '(2 3 4))

(define (find-min lst l)
  (define (helper predicate min-el)
    (if (null? predicate)
        min-el
        (if (< (minimum (first predicate) l) min-el)
            (helper (rest predicate) (minimum (first predicate) l))
            (helper (rest predicate) min-el)
            )
        )
    )
  (helper (rest lst) (minimum (first lst) l)))

(find-min (list (lambda (x) (+ x 1)) (lambda (x) (* x 2)) (lambda (x) (expt x 3))) '(2 3 4))
(find-min (list (λ (x) (+ x 2)) (λ (x) (sqrt x))) '(2 3 4))

(define (maxmin fm l)
  (define (helper matrix max-el)
    (if (null? matrix)
        max-el
        (if (> max-el (find-min (first matrix) l))
            (helper (rest matrix) max-el)
            (helper (rest matrix) (find-min (first matrix) l))
            )
        )
    )
  (helper (rest fm) (find-min (first fm) l)))

(maxmin (list (list (lambda (x) (+ x 1)) (lambda (x) (* x 2)) (lambda (x) (expt x 3))) (list (λ (x) (+ x 2)) (λ (x) (sqrt x)))) '(2 3 4))

(define (tranpose xss)
  (apply map list xss))

(define (zero-rows xss)
  (map (λ (xs) (if (member 0 xs) (map (curry * 0) xs) xs)) xss))

(define (zero-colls xss)
  (tranpose (zero-rows (tranpose xss)))
  )


(zero-colls '((1 2 0)
              (3 4 1)
              (0 5 7)
              (4 2 4))) ;'((0 2 0)
                         ; (0 4 0)
                         ; (0 5 0)
                         ; (0 2 0)))

(zero-rows '((2 1 2 4)
             (0 2 0 1)
             (4 4 1 4) 
             (4 1 3 1)))

