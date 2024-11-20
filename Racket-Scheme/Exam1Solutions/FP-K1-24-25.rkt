#lang racket
; Task 1 Solution
(define (find-start player-map)
  (cons (ormap (λ (row) (index-of row "S"))
               (apply map list player-map))
        (ormap (λ (row) (index-of row "S"))
               player-map)))

"Task 1 Tests"
(equal? (find-start '(("." "." ".")
                      ("." "." ".")
                      ("." "S" ".")
                      ("." "." ".")))
        '(2 . 1))

(equal? (find-start '(("." "." "F" "J")
                      ("S" "-" "|" ".")
                      ("." "." "J" ".")))
        '(1 . 0))


; Task 2 Solution
(define (get-next-steps player-map current-position)
  (define x (cdr current-position))
  (define y (car current-position))
  (define cols (length (first player-map)))
  (define rows (length player-map))
  (define (in-map? p)
    (and (0 . <= . (car p))
         ((car p) . < . rows)
         (0 . <= . (cdr p))
         ((cdr p) . < . cols)))
  (define (calc-delta dx dy)
    (define new-coords
      (cons (+ y dy) (+ x dx)))
    (and (in-map? new-coords) new-coords))
  (define str
    (list-ref (list-ref player-map y)
              x))
  (define next-steps
    (cond [(equal? str ".") '()]
          [(equal? str "-") (list (calc-delta -1  0)
                                  (calc-delta  1  0))]
          [(equal? str "|") (list (calc-delta  0 -1)
                                  (calc-delta  0  1))]
          [(equal? str "L") (list (calc-delta  0 -1)
                                  (calc-delta  1  0))]
          [(equal? str "J") (list (calc-delta  0 -1)
                                  (calc-delta -1  0))]
          [(equal? str "7") (list (calc-delta  0  1)
                                  (calc-delta -1  0))]
          [(equal? str "F") (list (calc-delta  0  1)
                                  (calc-delta  1  0))]
          [(equal? str "S") (list (calc-delta  0 -1)
                                  (calc-delta  0  1)
                                  (calc-delta  1  0)
                                  (calc-delta -1  0))]))
  (filter values next-steps))

"Task 2 Tests"
(define (check? returned target)
  (define (sort-pairs p-lst)
    (sort p-lst
          (λ (a b) (or (< (car a) (car b))
                       (and (= (car a) (car b))
                            (< (cdr a) (cdr b)))))))
  (equal? (sort-pairs target) (sort-pairs returned)))

(check? (get-next-steps '(("." "." ".")
                          ("." "." ".")
                          ("." "S" ".")
                          ("." "." "."))
                        '(2 . 1))
        '((1 . 1) (3 . 1) (2 . 2) (2 . 0)))

(check? (get-next-steps '(("." "." "F" "J")
                          ("S" "-" "|" ".")
                          ("." "." "J" "."))
                        '(1 . 2))
        '((0 . 2) (2 . 2)))

(check? (get-next-steps '(("." "." "F" "J")
                          ("S" "-" "|" ".")
                          ("." "." "J" "."))
                        '(0 . 3))
        '((0 . 2)))

(check? (get-next-steps '(("." "." "." ".")
                          ("S" "." "-" ".")
                          ("." "." "." "."))
                        '(1 . 2))
        '((1 . 1) (1 . 3)))

(check? (get-next-steps '(("." "." "." ".")
                          ("S" "." "|" ".")
                          ("." "." "." "."))
                        '(1 . 2))
        '((0 . 2) (2 . 2)))

(check? (get-next-steps '(("." "." "." ".")
                          ("S" "." "7" ".")
                          ("." "." "." "."))
                        '(1 . 2))
        '((1 . 1) (2 . 2)))

(check? (get-next-steps '(("." "." "." ".")
                          ("S" "." "L" ".")
                          ("." "." "." "."))
                        '(1 . 2))
        '((1 . 3) (0 . 2)))

(check? (get-next-steps '(("." "." "." ".")
                          ("S" "." "J" ".")
                          ("." "." "." "."))
                        '(1 . 2))
        '((1 . 1) (0 . 2)))

(check? (get-next-steps '(("." "." "." ".")
                          ("S" "." "F" ".")
                          ("." "." "." "."))
                        '(1 . 2))
        '((1 . 3) (2 . 2)))

(check? (get-next-steps '(("." "." "." ".")
                          ("S" "." "." ".")
                          ("." "." "." "."))
                        '(1 . 2))
        '())


; Task 3 Solution
(define (num-steps-farthest player-map)
  (define (expand-path path)
    (filter values
            (map (λ (coords) (if (member coords path)
                                 #f
                                 (cons coords path)))
                 (get-next-steps player-map (first path)))))
  (define (expand-paths paths)
    (filter (compose not null?)
            (apply append (map expand-path paths))))
  (define (count-steps steps paths)
    (if (null? paths)
        steps
        (count-steps (add1 steps)
                     (expand-paths paths))))
  (count-steps -1
               (list (list (find-start player-map)))))

"Task 3 Tests"
(= (num-steps-farthest '(("." ".")
                         ("S" ".")
                         ("." ".")))
   1)

(= (num-steps-farthest '(("." "." ".")
                         ("S" "-" "|")
                         ("." "." ".")))
   3)

(= (num-steps-farthest '(("." "." "F" "J")
                         ("S" "-" "|" ".")
                         ("." "." "J" ".")))
   4)

(= (num-steps-farthest '(("." "." "." ".")
                         ("." "." "F" "J")
                         ("S" "-" "|" ".")
                         ("." "." "J" ".")))
   5)

(= (num-steps-farthest '(("." "." "." "|")
                         ("." "." "F" "J")
                         ("S" "-" "|" ".")
                         ("." "." "J" ".")))
   5)

(= (num-steps-farthest '(("F" "-" "7" "." "." "|")
                         ("|" "." "|" "." "F" "J")
                         ("|" "." "S" "-" "|" ".")
                         ("|" "." "." "." "|" ".")
                         ("L" "-" "-" "." "J" ".")))
   11)