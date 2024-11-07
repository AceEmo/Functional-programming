#lang racket
#|
Реализирайте процедура zero-cols, която приема матрица xss и връща модифицирана версия на същата матрица, при която,
ако дадена колона на оригиналната матрица съдържа поне една 0, то в модифицираната матрица цялата колона ще се състои само от 0.
|#
(define (transpose xs)
  (apply map list xs))

(define (zero-rows xss)
  (map (λ (xs) (if (list? (member 0 xs)) (map (λ (x) 0) xs) xs)) xss)
  )

(define (zero-cols xss)
  (transpose (zero-rows (transpose xss)))
  )

(equal? (zero-cols '((1 2 0)
                     (3 4 1)
                     (0 5 7)
                     (4 2 4))) '((0 2 0)
                                 (0 4 0)
                                 (0 5 0)
                                 (0 2 0)))
