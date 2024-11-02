#lang racket
(require racket/trace)
;TASK1
(define (is-descending? n)
  (cond
    [(< n 10) #t]
    [else (and (>= (remainder (quotient n 10) 10) (remainder n 10)) (is-descending? (quotient n 10)))])
  )

(define (is-ascending? n)
  (cond
    [(< n 10) #t]
    [else (and (<= (remainder (quotient n 10) 10) (remainder n 10)) (is-ascending? (quotient n 10)))])
  )

(define (sum-numbers a b)
  (define (helper start end sum)
    (if (> start end)
      sum
      (if (is-descending? start)
          (helper (add1 start) end (+ sum start))
          (helper (add1 start) end sum)
          )
      )
    )
  (helper (min a b) (max a b) 0))

(equal? (sum-numbers 1 9) 45)
(equal? (sum-numbers 199 203) 200)
(equal? (sum-numbers 219 225) 663)

;TASK2
(define (bigger-elements first-el xs counter)
  (if (null? xs)
      counter
      (if (< first-el (first xs))
          (bigger-elements first-el (rest xs) (add1 counter))
          (bigger-elements first-el (rest xs) counter))))

(define (num-bigger-elements lst)
  (define (helper xs new-list)
     (if (null? xs)
      new-list
      (helper (rest xs) (append new-list (list (list (first xs) (bigger-elements (first xs) lst 0))))
      ))
    )
  (helper lst '()))

(equal? (num-bigger-elements '(5 6 3 4)) '((5 1) (6 0) (3 3) (4 2))) 
(equal? (num-bigger-elements '(1 1 1)) '((1 0) (1 0) (1 0)))

;TASK3
(define (switchsum f g n)
  (λ (x)
    (if (zero? n)
        0
        (+ (f x) ((switchsum g f (sub1 n)) (f x))))
    )
  )

(equal? ((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 1) 2) 3)
(equal? ((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 2) 2) 9)
(equal? ((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 3) 2) 16) 
(equal? ((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 4) 2) 30)

;TASK4
(define (repeater str)
  (define repeat-helper
    (λ (count glue)
      (if (zero? count)
          ""
          (if (= count 1)
              str
              (string-append str glue (repeat-helper (sub1 count) glue))))))
  repeat-helper)

(equal? ((repeater "I love Racket") 3 " ") "I love Racket I love Racket I love Racket")
(equal? ((repeater "Quack") 5 "!") "Quack!Quack!Quack!Quack!Quack")

(define (sum-digits n)
  (if(zero? n)
     0
     (+ (remainder n 10) (sum-digits (quotient n 10)))
     )
  )

(define (is-ok? n k)
  (zero? (remainder (sum-digits n) k))
  )

;TASK5
(define (sum-sum-digit a b k)
  (if (> a b)
      0
      (if (is-ok? a k)
          (+ a (sum-sum-digit (add1 a) b k))
          (sum-sum-digit (add1 a) b k))
      )
  )

;TASK6
(define (longest xs ys)
  (if (> (length xs) (length ys)) xs ys))

(define (max-ordered-sublist xs)
  (define (ordered-helper lst max-lst new-lst)
  (if (= (length lst) 1)
      (longest max-lst new-lst)
      (if (<= (first lst) (second lst))
        (ordered-helper (rest lst)  max-lst (append new-lst (list (second lst))))
        (ordered-helper (rest lst) (longest max-lst new-lst) (list (second lst)))
        )
      ))
  (ordered-helper xs '() (cons (car xs) '())))

(equal? (max-ordered-sublist '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))

;TASK7
(define (check-predicates x xs)
  (if (null? xs)
      #t
      (if ((first xs) x)
          (check-predicates x (rest xs))
          #f
          )
      )
  )

(define (where list-elements list-predicates)
  (define (helper xs new-lst)
  (if (null? xs)
      new-lst
      (if (check-predicates (first xs) list-predicates)
          (helper (rest xs) (append new-lst (list (first xs))))
          (helper (rest xs) new-lst)
          )
      )
    )
  (helper list-elements '()))

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10))

;TASK8
(define (set-union xs ys)
  (if (null? xs)
      (sort ys <)
      (if (list? (member (first xs) ys))
          (set-union (rest xs) ys)
          (set-union (rest xs) (append ys (list (first xs))))
          )
      )
  )

(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))