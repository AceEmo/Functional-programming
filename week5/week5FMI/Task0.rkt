#lang racket
(define (fib n)
  (define (helper i prev cur)
    (if (>= i n)
        cur
        (helper (+ i 1) cur (+ prev cur))))
  (helper 0 0 1))

(define (to-list n)
  (define (helper res k)
    (if (< k 10)
        (cons k res)
        (helper (cons (remainder k 10) res)
                (quotient k 10))))
  (helper '() n))

(to-list (fib 179))

(define (to-groups lst k)
  (if (< (length lst) k)
      (list lst)
      (cons (take lst k)
            (to-groups (drop lst k) k))))

(define (count lst)
  (map (λ (k) (cons k
                    (length (filter (curry = k) lst))))
       (range 10)))

(count '(1 8 5 4 7 7 0 7 6 8 9 4 7 1 9 8 6 2 1 2 1 9 0 1 3))

(define (max-pair pairs)
  (argmax cdr pairs))

(max-pair (count '(1 8 5 4 7 7 0 7 6 8 9 4 7 1 9 8 6 2 1 2 1 9 0 1 3)))

(define (around-fib n)
  (λ (k) (map (compose max-pair count)
              (to-groups (to-list (fib (- n 1))) k))))

; 18547707689471986212190138521399707760
; 1854770768947198621219013 8521399707760
; '((1 . 5)                 (7 . 3))
((around-fib 100) 25)   ; -> '((1 . 3))
((around-fib 180) 25)   ; -> '((1 . 5) (7 . 3))
((around-fib 1700) 25)  ; -> '((1 . 4) (2 . 5) (0 . 6) (4 . 5)
                        ;      (5 . 7) (2 . 4) (6 . 7) (3 . 5)  
                        ;      (0 . 4) (8 . 5) (4 . 5) (4 . 4)
                        ;      (7 . 7) (7 . 6) (2 . 2))
((around-fib 500) 42)   ; -> '((0 . 6) (2 . 7) (2 . 6))
((around-fib 6000) 242) ; -> '((5 . 31) (8 . 33) (8 . 31)
                        ;      (7 . 35) (7 . 31) (4 . 7))