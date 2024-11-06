#lang racket
(require racket/trace)
(define (my-flatten-test xss)
  (cond
    [(null? xss) null]
    [(not (list? (car xss))) (cons (car xss) (my-flatten (cdr xss)))]
    [else (append (my-flatten (car xss)) (my-flatten (cdr xss)))]
    )
  )

(define (my-flatten xss)
  (if (null? xss)
      null
      (if (not (list? (first xss)))
          (cons (first xss) (my-flatten (rest xss)))
          (append (my-flatten (first xss)) (my-flatten (rest xss)))
          )
      )
  )

(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))
(equal? (my-flatten '(422 22 ((((11))) 33) (131 31 121 12121 11 (12555 555)))) '(422 22 11 33 131 31 121 12121 11 12555 555))