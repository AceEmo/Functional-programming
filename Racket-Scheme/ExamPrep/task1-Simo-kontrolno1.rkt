#lang racket

(define (isNumOdd? n)
  (if (zero? n)
      #t
      (if (odd? (remainder n 10))
          (isNumOdd? (quotient n 10))
          #f
          )
      )
  )

(define (revNum n)
  (define (helper rev num)
    (if (zero? num)
        rev
        (helper (+ (* rev 10) (remainder num 10)) (quotient num 10))
        )
    )
  (helper 0 n))

(define (isValid? n)
  (if (zero? n)
      #t
      (if (zero? (remainder n 10))
          #f
          (isValid? (quotient n 10))
          )
      )
  )

(define (reversibleNumbers n)
  (define (helper xs counter)
    (if (> counter n)
        xs
        (if (and (isValid? counter) (isNumOdd? (+ counter (revNum counter))))
            (helper (append xs (list counter)) (add1 counter))
            (helper xs (add1 counter))
            )
        )
    )
  (helper '() 1))

(reversibleNumbers 20)
(reversibleNumbers 31)
(reversibleNumbers 10)