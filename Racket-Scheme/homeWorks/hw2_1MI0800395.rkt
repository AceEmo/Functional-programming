#lang racket
;TASK1
(define (find-start player-map)
  (cons (ormap (λ (row) (index-of row "S"))
               (apply map list player-map))
        (ormap (λ (row) (index-of row "S"))
               player-map)))
;TASK2
(define (get-next-steps player-map current-position)
  (define x (cdr current-position))
  (define y (car current-position))
  (define cols (length (first player-map)))
  (define rows (length player-map))
  
  (define (in-map? p)
    (and (>= (car p) 0) (< (car p) rows)
         (>= (cdr p) 0) (< (cdr p) cols)))

  (define (calc-delta dx dy)
    (define new-coords (cons (+ y dy) (+ x dx)))
    (and (in-map? new-coords) new-coords))

  (define str (list-ref (list-ref player-map y) x))
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

;TASK4
(define (expand-path path visited player-map)
  (define current-position (first path))
  (define next-steps
    (filter (λ (coords) (not (member coords visited)))
            (get-next-steps player-map current-position)))
  (map (λ (coords) (cons coords path)) next-steps))

(define (expand-paths paths visited player-map)
  (apply append
         (map (λ (path)
                (expand-path path visited player-map))
              paths)))

(define (count-steps steps paths visited player-map)
  (if (null? paths)
      steps
      (let ([new-visited (append visited (map first paths))])
        (count-steps (add1 steps) (expand-paths paths new-visited player-map) new-visited player-map))))

(define (num-steps-farthest-away player-map)
  (let ([start (find-start player-map)])
    (count-steps -1 (list (list start)) (list start) player-map)))

(equal? (num-steps-farthest-away '(("7" "-" "F" "7" "-")
                           ("." "F" "J" "|" "7")
                           ("S" "J" "L" "L" "7")
                           ("|" "F" "-" "-" "J")
                           ("L" "J" "." "L" "J"))) 8)

(equal? (num-steps-farthest-away '(("-" "." "|" "F" "7")
                            ("." "S" "-" "7" "|")
                            ("L" "|" "7" "|" "|")
                            ("-" "L" "-" "J" "|")
                            ("L" "|" "-" "J" "F"))) 4)

(equal? (num-steps-farthest-away '(("F" "-" "-" "7")
                            ("|" "F" "-" "7")
                            ("." "S" "." "|")
                            ("|" "L" "-" "J")
                            ("L" "-" "-" "J"))) 4)
