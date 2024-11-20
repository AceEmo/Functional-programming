#lang racket

(define (my-length-rec xs)
  (if (null? xs)
      0
      (add1 (my-length-rec (cdr xs)))
      )
  )

; using a recursive procedure
(= (my-length-rec '()) 0)
(= (my-length-rec '(1 2 5 6 4 8)) 6)