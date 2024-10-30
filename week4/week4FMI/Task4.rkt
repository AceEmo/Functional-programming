#lang racket

(foldr + 0 '(1 2 3 4))

(foldr cons '() '(1 2 3 4))

(define (get-smallest-rec xs)
  (foldr min (car xs) xs)
  )


(= (get-smallest-rec '(1 2 5)) 1)
(= (get-smallest-rec '(2 1 5)) 1)
(= (get-smallest-rec '(2 1 -1 5)) -1)