#lang racket

(define (even-if? a)
  (if (equal? (remainder a 2) 0)
      "Yes"
      "No"))

(define (sum-cubes-pow a b)
  (+ (* a (* a a)) (* b (* b b)))
  )

(define (sq-avg a b)
  (/ (+ (* a a)(* b b)) 2)
  )

(define (my-gcd a b)
  (cond
    ([zero? a] b)
    ([zero? b] a)
    ([equal? a b] a)
    ([> a b] (my-gcd (- a b) b))
    ([< a b] (my-gcd a (- b a)))
    )
  )

(define (leap-year-guards? year)
  (cond
    [(zero? (remainder year 400)) #t] 
    [(zero? (remainder year 100)) #f]
    [(zero? (remainder year 4))   #t]
    [else                         #f]))

(define (can-carry? c k w)
  (if (<= (* c w) k)
      #t
      #f)
  )

(define (growing-plant up-speed down-speed desired-height)
  (define (time-pass day current-height)
    (if (>= up-speed desired-height)
        1
    (if (>= current-height desired-height)
        day
        (time-pass (add1 day) (+ current-height up-speed (- down-speed))))))
(time-pass 0 0))

(define (rev a)
  (define (helper a b)
    (if (<= a 0)
        b
        (helper (quotient a 10) (+ (* b 10) (remainder a 10)))))
  (helper a 0))

(define (palindrome? a)
  (define (helper temp rev)
    (if (zero? temp)
        (equal? rev a)
        (helper (quotient temp 10) (+ (* rev 10) (remainder temp 10)))))
  (helper a 0))

(define (sum-digits-iter a)
  (define (helper temp sum)
    (if (zero? temp)
        sum
        (helper (quotient temp 10) (+ sum (remainder temp 10)))
        )
    )
  (helper a 0))

(define (sum-digits-rec a)
  (if (zero? a)
      0
      (+ (remainder a 10) (sum-digits-rec (quotient a 10)))))











