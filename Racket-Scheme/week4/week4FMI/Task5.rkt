#lang racket
 (require racket/trace)
(define (remove-first-proc x xs)
  ;(remove x xs)
  (if (null? xs)
      '()
      (if (equal? x (car xs))
          (cdr xs)
          (cons (car xs) (remove-first-proc x (cdr xs)))
          )
      )
  )
(trace remove-first-proc)

(equal? (remove-first-proc 1 '(1 1 1 2)) '(1 1 2))
(equal? (remove-first-proc 1 '(2 5 6)) '(2 5 6))




(equal? (remove-first-proc 1 '(1)) '())
(equal? (remove-first-proc 1 '(2 1)) '(2))
(equal? (remove-first-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN" "RNN"))