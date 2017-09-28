#lang racket
(module+ test (require rackunit))
(require "chapter-02.rkt"
         "chapter-03.rkt")


;; set? :: List -> Bool
(define set?
  (λ (l)
     (cond
       ((null? l) #t)
       ((member? (car l) (cdr l)) #f)
       (else (set? (cdr l))))))

(module+ test
  (check-false
    (set? '(apple peaches apple plum))
    "Dublicated apple makes it no set")

  (check-true
    (set? '(apple peaches pear plum))
    "All elements are distinct so it's a set"))



;; makeset :: List -> Set
(define makeset
  (λ (l)
     (cond
       ((null? l) '())
       (else
         (cons (car l)
               (makeset (multirember (car l) (cdr l))))))))

(module+ test
  (check-equal?
    (makeset '(apple peach pear peach
               plum apple lemon peach))
    '(apple peach pear plum lemon)
    "Apple and peach duplicates removed to make a set"))
