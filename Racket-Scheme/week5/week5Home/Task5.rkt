#lang racket
#|
Да се дефинира процедура diagonals, която приема матрица
и връща точкова двойка от вида ( <главен диагонал> . <второстепенен диагонал>).
|#

(define (diagonals xss)
  (define (helper matrix main-counter second-counter main-diagonal second-diagonal)
    (if (null? matrix)
        (cons main-diagonal second-diagonal)
        (helper (rest matrix) (add1 main-counter) (sub1 second-counter)
                (append main-diagonal (take (drop (first matrix) main-counter) 1))
                (append second-diagonal (take (drop (first matrix) second-counter) 1)))
        )
    )
  (helper xss 0 (sub1 (length (first xss))) '() '()))

(equal? (diagonals '((1 2 3) 
                     (4 5 6) 
                     (7 8 9))) '((1 5 9) . (3 5 7)))
(equal? (diagonals '((10 20) 
                     (30 40))) '((10 40) . (20 30)))
(equal? (diagonals '((1  2  3  4) 
                     (5  6  7  8) 
                     (9 10 11 12))) '((1 6 11) . (4 7 10)))
(equal? (diagonals '((100))) '((100) . (100)))
