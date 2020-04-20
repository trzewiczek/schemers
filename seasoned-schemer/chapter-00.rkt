#lang racket
(module+ test (require rackunit))
(provide atom? add1 sub1)


;; atom? :: _ -> Bool
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

(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))


;; add1 :: Number -> Number
(module+ test
  (check-equal?
    (add1 67) 68
    "It should add 1 to number n"))

(define add1
  (λ (n)
    (+ n 1)))


;; sub1 :: Number -> Number
(module+ test
  (check-equal?
    (sub1 5) 4
    "It should substitute 1 from number n"))

(define sub1
  (λ (n)
    (- n 1)))
