#lang racket

(define (num-mul x)
  (define (helper multy num)
    (if (zero? num)
        multy
        (helper (* multy (remainder num 10)) (quotient num 10))
        )
    )
  (helper 1 x))

(define (numbers a b)
  (λ (k)
    (define (helper sum a)
      (if (> a b)
          sum
          (if (zero? (remainder (num-mul a) k))
              (helper (+ sum a) (add1 a))
              (helper sum (add1 a))
              )
          )
      )
    (helper 0 a))
  )

((numbers 1 30) 3) ;→ 204 
((numbers 1 25) 2) ;→ 225 
((numbers 1 37) 6) ;→ 262 
((numbers 1 29) 9) ;→ 87 
((numbers 1 40) 7) ;→ 188 
((numbers 1 5)  7) ;→ 0 