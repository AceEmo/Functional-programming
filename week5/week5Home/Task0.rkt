#lang racket
#|
Да се дефинира функция (diagonal mat), която получава квадратна числова матрица mat, представена като списък от списъци,
и връща списък от елементите на главния диагонал на матрицата.
|#

(require racket/trace)

(define (diagonal xss)
  (define (helper matrix lst counter)
    (if (null? matrix)
        lst
        (helper (rest matrix) (append lst (take (drop (first matrix) counter) 1)) (add1 counter))
        )
    )
  (helper xss '() 0))

(equal? (diagonal '((1 2 3 4)
                    (5 6 7 8)
                    (9 10 11 12)
                    (13 14 15 16))) '(1 6 11 16))