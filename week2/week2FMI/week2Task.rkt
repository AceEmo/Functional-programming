#lang racket
(define (pow-rec x n)
  (if (zero? n)
      1
      (* x (pow-rec x (sub1 n)))
      )
  )

(define (pow-iter x n)
  (define (helper n result)
    (if (zero? n)
        result
        (helper (sub1 n) (* result x))
        )
    )
  (helper n 1))

(require math/number-theory)

(define (sum-divs n)
  (define (helper i)
    (if (zero? i)
        0
        (if (divides? i n)
            (+ i (helper (sub1 i)))
            (helper (sub1 i))
            )
        )
    )
  (if (negative? n)
      0
      (helper n)
      )
  )

(define (inc-digits? n)
  (if (>= (abs n) 10)
      (and (< (remainder (quotient n 10) 10) (remainder n 10)) (inc-digits? (quotient n 10)))
      #t
      )
  )

(require racket/trace)

(define (num-digits n)
  (if (< n 10)
      1
      (add1 (num-digits (quotient n 10)))
      )
  )

(define (narcissistic? n)
  (define (helper len current)
    (if (positive? current)
        (+ (expt (remainder current 10) len) (helper len (quotient current 10)))
        0
        )
    )
  (trace helper)
  (= (helper (num-digits n) n) n)
  )

;(equal? (narcissistic? 7) #t)
;(equal? (narcissistic? 12) #f)
;(equal? (narcissistic? 153) #t)
;(equal? (narcissistic? 370) #t)
;(equal? (narcissistic? 371) #t)
;(equal? (narcissistic? 1634) #t)


(define (find-max n)
  (if (< (abs n) 10)
      (abs n)
      (max (remainder (abs n) 10) (find-max (quotient n 10)))
      )
  )

(define (my-sin-rec n x)
  (if (negative? n)
      0
      (+ (/ (* (expt -1 n) (expt x (add1 (* n 2))) ) (factorial (add1 (* n 2) ) ) ) (my-sin-rec (sub1 n) x) )
      )
  )

(define (my-sin-iter n x)
  (define (helper i result)
    (if (<= i n)
        (helper (add1 i) (+ (/ (* (expt -1 n) (expt x (add1 (* n 2))) ) (factorial (add1 (* n 2) ) ) )
        result
        )
    )
  (helper 0 0)
  )

(= (my-sin-iter 100 1.570796) 0.9999999999999465) ; 90 degrees => 0.9999999999999465
(= (my-sin-iter 100 0.5235988) 0.5000000211324931) ; 30 degrees => 0.5000000211324931
