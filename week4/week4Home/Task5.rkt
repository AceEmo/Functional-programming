#lang racket

(define (find-path matrix)
  (define rows (length matrix))
  (define cols (length (first matrix)))

  ;; Проверка дали дадена клетка е извън границите или е препятствие
  (define (out-of-bounds-or-blocked? row col)
    (or (< row 0) (>= row rows)
        (< col 0) (>= col cols)
        (= 1 (list-ref (list-ref matrix row) col))))

  ;; Рекурсивна функция за намиране на път
  (define (search row col visited)
    (cond
      ;; Ако сме достигнали крайната позиция, връщаме истина
      [(and (= row (- rows 1)) (= col (- cols 1))) #t]

      ;; Ако е извън границите, е препятствие или вече е посетена
      [(or (out-of-bounds-or-blocked? row col)
           (set-member? visited (list row col))) #f]

      ;; Продължаваме да търсим, маркирайки текущата клетка като посетена
      [else
       (let ([new-visited (set-add visited (list row col))])
         (or (search (+ row 1) col new-visited) ; Надолу
             (search (- row 1) col new-visited) ; Нагоре
             (search row (+ col 1) new-visited) ; Надясно
             (search row (- col 1) new-visited)))])) ; Наляво

  ;; Започваме търсенето от горния ляв ъгъл с празно множество за посетените клетки
  (search 0 0 (set)))

(define matrix '((0 1 1)
                 (0 0 1)
                 (1 0 0)
                 (0 0 0)
                 (0 1 1)
                 (0 0 0)))

(find-path matrix) ; -> #t (тъй като има път от (0, 0) до (2, 2))