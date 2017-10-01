#lang racket
(require "chapter-01.rkt")
(module+ test (require rackunit))
(provide member?)


;; lat? :: List -> Bool
(module+ test
  (check-true
    (lat? '(Jack Sprat could eat no chicken fat))
    "List of strings is a list of atoms")

  (check-false
    (lat? '((Jack) Sprat could eat no chicken fat))
    "One of S-expressions in l is not an atom")

  (check-false
    (lat? '(Jack (Sprat could) eat no chicken fat))
    "One of S-expressions in l is not an atom")

  (check-true
    (lat? '())
    "Empty list is a list of atoms"))

(define lat?
  (λ (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))



;; member? :: Atom -> List Atom -> List Atom
(module+ test
  (check-true
    (member? 'tea '(coffee tea or milk))
    "Tea is present in the list")

  (check-false
    (member? 'poached '(fried eggs and scrambledd eggs))
    "poached is not present in the list"))

(define member?
  (λ (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))


