#lang racket
(define (spiral xss)
  (define (helper matrix counter result)
    (if (null? matrix)
        result
        (if (even? counter)
            (helper (rest matrix) (add1 counter) (append result (first matrix)))
            (helper (rest matrix) (add1 counter) (append result (reverse (first matrix))))
            )
        )
    )
  (helper xss 0 '()))

(equal? (spiral '((1 2 3 4)
                  (5 6 7 8)
                  (9 10 11 12)
                  (13 14 15 16))) '(1 2 3 4 8 7 6 5 9 10 11 12 16 15 14 13))