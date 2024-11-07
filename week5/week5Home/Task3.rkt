#lang racket
(define (string-to-char-list str)
  (map (lambda (ch) (string ch)) (string->list str)))

(define (char-list-to-string xs)
  (apply string-append xs))

(define (longest str1 str2)
  (char-list-to-string (sort (remove-duplicates (append (string-to-char-list str1) (string-to-char-list str2))) string<?))
  )

(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")