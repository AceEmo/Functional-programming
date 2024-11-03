#lang racket
(require racket/trace)

;TASK1
(define (find-max xs)
  (define (helper max-el lst)
    (if (null? lst)
        max-el
        (if (> (first lst) max-el)
            (helper (first lst) (rest lst))
            (helper max-el (rest lst))
            )
        )
    )
  (helper (first xs) (rest xs)))

(define (right-max xs)
  (define (helper lst new-lst max-el)
    (if (= (length lst) 1)
        (append new-lst (list max-el))
        (if (eq? (first lst) max-el)
            (helper (rest lst) (append new-lst (list max-el)) (find-max (rest lst)))
            (helper (rest lst) (append new-lst (list max-el)) max-el)
            )
        )
    )
  (helper xs '() (find-max xs)))


(equal? (right-max '(1 2 3 4 -5 6 7 -2 -1 0)) '(7 7 7 7 7 7 7 0 0 0))
(equal? (right-max '(5 8 9 12)) '(12 12 12 12))           
(equal? (right-max '(4 3 2 1 0)) '(4 3 2 1 0))


;TASK2
(define (kth-number xs)
  (λ (p k)
    (define (find-k lst count)
      (if (eq? count k)
          (first lst)
          (find-k (rest lst) (add1 count))))
    
    (define filtered-and-sorted (sort (filter p xs) <))
    
    (if (>= (length filtered-and-sorted) k)
        (find-k filtered-and-sorted 1)
        (error "No such number"))
    )
  )

(equal? ((kth-number '(1 2 3 4 -5 6))       odd? 2)       1) 
(equal? ((kth-number '(1 2 3 4 -5 6))       negative? 1) -5) 
(equal? ((kth-number '(1 2 3 4 -5 -5 6))    negative? 2) -5)
(equal? ((kth-number '(1 -4 2 3 4 -5 -5 6)) negative? 3) -4) 
;((kth-number '(-1 0 -1 0 -2)) negative? 4) error "No such number"

;TASK3
(define (num-to-xs n)
  (define (helper xs num)
    (if (zero? num)
        xs
        (helper (cons (remainder num 10) xs) (quotient num 10))
        )
    )
  (helper '() n))

(define (xs-to-num lst)
  (define (helper xs num)
    (if (null? xs)
        num
        (helper (rest xs) (+ (* num 10) (first xs)))
        )
    )
  (helper lst 0))

(define (palindromize n)
  (define rem-xs (remove-duplicates (sort (num-to-xs n) <)))
  (define (helper xs rev-xs)
    (xs-to-num (append xs rev-xs))
        )
  (helper rem-xs (reverse rem-xs)))

(equal? (palindromize 11) 11)
(equal? (palindromize 3354457878) 3457887543)
(equal? (palindromize 11335445789789) 13457899875431)

;TASK4
(define (fib-n n)
  (define (helper n a b)
    (if (zero? n)
        a
        (helper (sub1 n) b (+ a b))
        )
    )
  (if (negative? n)
      (error "Negative!")
      (helper n 0 1)))

(equal? (fib-n 100) 354224848179261915075) ;21 при k=25, става (1.3)
(equal? (fib-n 180) 18547707689471986212190138521399707760) ;38, при k=25, става (1.5) (7.3)


(define (max-occur num)
  (define xs (num-to-xs num))
  (define (helper lst max-oc curr-oc max-el curr-el)
    (cond
      ((null? lst)
       (if (> curr-oc max-oc)
           (cons curr-el curr-oc)
           (cons max-el max-oc)))
      ((eq? (first lst) curr-el)
       (helper (rest lst) max-oc (add1 curr-oc) max-el curr-el))
      (else
       (define new-max (if (> curr-oc max-oc) 
                           (cons curr-el curr-oc)
                           (cons max-el max-oc)))
       (helper (rest lst) (cdr new-max) 1 (first lst) (first lst)))))
  
  (trace helper)
  (let ((initial-el (first xs)))
    (helper (rest xs) 1 1 initial-el initial-el)))