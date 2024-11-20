#lang racket
#|
Да се дефинира предикат triangular?, който получава квадратна числова матрица и проверява дали тя е горно триъгълна,
т.е. дали всички елементи под главния ѝ диагонал са нули.
|#
(define (zeroes? xs len)
  (if (zero? len)
      #t
      (if (zero? (first xs))
          (zeroes? (rest xs) (sub1 len))
          #f
          )
      )
  )

(define (triangular? xss)
  (define (helper matrix counter)
    (if (null? matrix)
        #t
        (if (zeroes? (first matrix) counter)
            (helper (rest matrix) (add1 counter))
            #f
            )
        )
    )
  (helper xss 0))

(equal? (triangular? '((1 2 3)
                       (0 5 6)
                       (0 0 9))) #t)

(equal? (triangular? '((0 2 3)
                       (0 0 6)
                       (1 0 0))) #f)

(equal? (triangular? '((1 2 3)
                       (1 5 6)
                       (0 0 9))) #f)

(equal? (triangular? '((1 2 3 4)
                       (0 5 6 7)
                       (0 0 8 9)
                       (0 0 0 9))) #t)