#lang racket
(require racket/trace)
#|
Description:
Define a procedure that takes a matrix xss,
and returns a modified version of it modified-xss in which if a row in xss has a zero then the whole row in modified-xss contains only zeros.
|#
(define (zero-row? xs)
  (if (null? xs)
      #f
      (if (zero? (first xs))
          #t
          (zero-row? (rest xs))
          )
      )
  )

(define (make-zero-row lst)
  (define (helper xs new-xs)
    (if (null? xs)
        new-xs
        (helper (rest xs) (append new-xs (list 0)))
        )
    )
  (helper lst '()))


(define (zero-rows xss)
  (define (helper matrix new-matrix)
    (if (null? matrix)
        new-matrix
        (if (zero-row? (first matrix))
            (helper (rest matrix) (append new-matrix (list (make-zero-row (first matrix)))))
            (helper (rest matrix) (append new-matrix (list (first matrix))))
            )
        )
    )
  (helper xss '()))

(equal? (zero-rows '((1 2 0) (3 4 1) (0 5 7) (4 2 4))) '((0 0 0) (3 4 1) (0 0 0) (4 2 4)))
(equal? (zero-rows '((2 1 2 4) (0 2 0 1) (4 4 1 4) (4 1 3 1))) '((2 1 2 4) (0 0 0 0) (4 4 1 4) (4 1 3 1)))
