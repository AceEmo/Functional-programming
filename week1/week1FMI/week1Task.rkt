#lang racket

;hello world
"hello world"

;min-if uses an if-else statement.
(define (min-if input1 input2)
  (if (> input2 input1) input1 input2))

;min-guard uses guards.
(define (min-guard input1 input2)
  (cond
    [(< input1 input2) input1]
    [else input2]))

;min-built-in uses built-in functions.
(define (min-built-in input1 input2)
  (min input1 input2))

;last-digit: returns the last digit of a number;
(define (last-digit input)
  (modulo input 10))

;quotient-whole: returns the quotient of the division of two numbers;
(define (quotient-whole input1 input2)
  (quotient input1 input2))

;div-whole: returns the quotient and remainder of the division of two numbers;
(define (div-whole input1 input2)
  (quotient/remainder input1 input2))

;remove-last-digit: returns a whole number without its last digit;
(define (remove-last-digit input)
  (quotient input 10))

;avg-whole: returns the average of two whole numbers;
(define (average-whole input1 input2)
  (/ (+ input1 input2) 2))

;round-two-dig: return the number rounded to the second digit after the decimal point.
(define (round-two-dig input)
  (/ (round (* input 100)) 100.0))

;fact-rec creates a linearly recursive process.
(define (fact-rec input)
  (if (zero? input) 1
      (* input (fact-rec (sub1 input)))))

;fact-iter creates a linearly iterative process.
(define (fact-iter input)
  (define (iter product input)
    (if (zero? input) product
        (iter (* product input) (sub1 input))))
  (iter 1 input))


(define (min-iff x y)
  (if (< x y)
      x
      y
      )
  )

(define (min-guardd x y)
  (cond
    [(< x y) x]
    [(= x y) x]
    [else y])
  )

(define (min-build-in x y)
  (min x y)
   )

(define (last-digitt x)
  (remainder x 10)
  )

(define (quotient-wholee x y)
  (quotient x y)
  )

(define (div-wholee x y)
  (/ x y)
  )

(define (remove-last-digite x)
  (quotient x 10)
  )

(define (average-wholee x y)
  (/ (+ x y) 2)
  )

(define (round-two-digit x)
  (/ round(* x 100) 100)
  )

(define (fact-recc n)
  (if (zero? n)
      1
      (* n fact-rec (sub1 n))
      )
  )

(define (fact-iterr n)
  (define (fact-helper n res)
    (if (zero? n)
        res
        (fact-helper (sub1 n) (* n res))
    )
    )
  (if (negative? n)
      (error "N is negative")
  (fact-helper n 1)
  )
  )

(define (fib-rec n)
  (cond
    [(negative? n) (error "Negative!")]
    [(< n 2) n]
    [else (+ (fib-rec (sub1 n)) (fib-rec (- n 2)))]
    )
  )

(define (fib-iter n)
  (define (helper n a b)
    (if (zero? n)
        a
        (helper (sub1 n) b (+ a b))
        )
    )
  (if (negative? n)
      (error "Negative!")
      (helper n 0 1)
      )
  )


(define (not-equal-one-line? x y)
  (not (= x y))
  )

(define (not-equal-guards? x y)
  (cond
    [(= x y) #f]
    [else #t]
    )
  )

(define (inside? a b x)
  (and (<= (min a b) x) (<= x (max a b))) ;(<= (min a b) x (max a b))
  )


(= (min-if -60 15) -60)
(= (quotient-whole 62 2) 31)
(equal? (not-equal-one-line? 5 2) #t)


(equal? (not-equal-one-line? 5 5) #f)

(equal? (not-equal-guards? 5 2) #t)
(equal? (not-equal-guards? 5 5) #f)

(equal? (inside? 1 5 4) #t) ; start = 1, finish = 5, x = 4
(equal? (inside? 5 1 4) #t)
(equal? (inside? 10 50 20) #t)
(equal? (inside? 10 50 1) #f)