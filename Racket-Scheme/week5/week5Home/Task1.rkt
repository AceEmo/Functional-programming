#lang racket
#|
Да се дефинира функция от по-висок ред (tabulate f), която връща като резултат функция.
Върнатата функция приема като аргументи целите числа a и b и връща като резултат списък от точкови двойки от вида '(x . fx),
първите елементи на които са поредните точки от интервала [a, b], а вторите – стойностите на f в тези точки. 
|#
(require racket/trace)
(define (tabulate f)
  (λ (a b)
    (define (helper a b lst)
      (if (> a b)
          lst
          (helper (add1 a) b (append lst (list (cons a (f a)))))
          )
      )
    (trace helper)
    (helper a b '()))
  )

(equal? ((tabulate sqr) 1 5) '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))