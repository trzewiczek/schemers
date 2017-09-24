#lang racket
(module+ test (require rackunit))
(provide atom?)


;; atom? :: _ -> Bool
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(module+ test (require rackunit)
  (check-true
    (atom? 'atom)
    "String is an atom")

  (check-true
    (atom? 'turkey)
    "String is an atom")

  (check-true
    (atom? 'u)
    "Single character is an atom")

  (check-true
    (atom? 1492)
    "Number is an atom")

  (check-true
    (atom? '*abc$)
    "String is an atom")

  (check-false
    (atom? '())
    "Empty list is not an atom")

  (check-false
    (atom? '(1 2 3))
    "Non-empty list is not an atom"))

