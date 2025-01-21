#lang racket
(define (helper-sum xs k sum)
  (if (zero? k)
      sum
      (helper-sum (rest xs) (sub1 k) (+ sum (first xs)))
      )
  )

(define (window-sum xs k)
  (define (helper max-sum lst)
    (if (< (length lst) k)
        max-sum
        (helper (if (< max-sum (helper-sum lst k 0)) (helper-sum lst k 0) max-sum) (rest lst))
        )
    )
  (helper 0 xs))

(window-sum '(1 2 3 1 3 2 0) 3)
(window-sum '(1 2 3 1 3 2 0) 1)
(window-sum '(1 2 3 1 3 2 0) 2)
(window-sum '(1 2 3 1 3 2 0) 4)