#lang racket

(require racket/trace)

(define (count-digits-iter n)
  (define (helper n counter)
    (if (zero? n)
        counter
        (helper (quotient n 10) (add1 counter))
        )
    )
  ;(trace helper)
  (helper n 0))

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)

(define (count-digits-rec n)
  (if (zero? n)
      0
      (add1 (count-digits-rec (quotient n 10)))
      )
  )
;(trace count-digits-rec)

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)

(define (sum-digits-iter n)
  (define (helper n sum)
    (if (zero? n)
        sum
        (helper (quotient n 10) (+ (remainder n 10) sum))
      )
    )
  ;(trace helper)
  (helper n 0))

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)

(require math/number-theory)

;; Helper function to check if a number is prime
(define (is-prime? n)
  (define (helper x)
    (if (> x 1)
        (or (divides? x n) (helper (sub1 x)))
        #f
     )
    )
  (if (negative? n)
      (error "Negative n")
      (and (> n 1) (not (helper (sub1 n))))
   )
  )

;; Helper function to remove all occurrences of a prime factor from n
(define (remove-prime-factors n divisor)
  (if (zero? (remainder n divisor))
      (remove-prime-factors (quotient n divisor) divisor)
      n
      )
  )
;(trace remove-prime-factors)

;; Recursive procedure to find the sum of prime divisors
(define (sum-prime-divs-rec n)
  (define (helper n divisor sum)
    (cond
      [(< n 2) sum] ;; Base case, no more divisors
      [(and (is-prime? divisor) (zero? (remainder n divisor))) (helper (remove-prime-factors n divisor) (add1 divisor) (+ sum divisor))]
      [else (helper n (add1 divisor) sum)])) ;; Move to the next divisor
  ;(trace helper)
  (helper n 2 0))

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)

(define (is-palindrome? a)
  (define (helper temp rev)
    (if (zero? temp)
        (equal? rev a)
        (helper (quotient temp 10) (+ (* rev 10) (remainder temp 10)))))
  (helper a 0))

(define (num-palindromes a b)
  (define left (min a b))
  (define right (max a b))
  (define (num-palindromes-rec left right)
    (if (> left right)
        0
        (if (is-palindrome? left)
            (add1 (num-palindromes-rec (add1 left) right))
            (num-palindromes-rec (add1 left) right))))
  (num-palindromes-rec left right))

(= (num-palindromes 1 101) 19)  ;; Should return 19 palindromes between 1 and 101
(= (num-palindromes 1 100) 18)  ;; Should return 18 palindromes between 1 and 100
(= (num-palindromes 100 1) 18)  ;; Should return 18 palindromes between 1 and 100 (range reversed)


(define (sum-divs n)
  (define (helper result d)
    (cond
      [(> d n) result]
      [(divides? d n) (helper (+ result d) (add1 d))]
      [else (helper result (add1 d))]
      )
    )
  (if (<= n 0)
      0
      (helper 1 2)))
;(trace sum-divs)

(define (amicable? a b)
  (if (eq? (sum-divs a) (sum-divs b))
      #t
      #f
     )
  )


(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)