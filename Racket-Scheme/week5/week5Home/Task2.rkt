#lang racket
(require racket/trace)

(define (sub-list? lst1 lst2)
  (define (helper xs1 xs2)
    (if (null? xs1)
        #t
        (if (<= (first xs1) (first xs2))
            (helper (rest xs1) (rest xs2))
            #f
            )
        )
    )
  (helper lst1 lst2))

(define (major? lst1 lst2 n)
  (define (helper xs1 xs2 len)
    (if (< (length xs2) len)
        #f
        (if (sub-list? xs1 (take xs2 len))
            #t
            (helper xs1 (rest xs2) len)
            )
        )
    )
  (helper lst1 lst2 n))

(define (is-major? xss)
  (define (helper matrix first-row len)
    (if (null? matrix)
        #t
        (if (major? first-row (first matrix) len)
            (helper (rest matrix) (first matrix) (length (first matrix)))
            #f
            )
        )
    )
  (helper (rest xss) (first xss) (length (first xss))))

(is-major? '((1 3) (4 2 7) (2 5 4 3 9 12)))
(is-major? '((1 3) (4 2 7) (2 5 3 3 9 12)))
(is-major? '((2 3 4) (1 3 5 6 7 0 1) (2 4 6 7 8 1 2 3) (0 0 0 3 5 7 9 9 3 4 10 0 0)))