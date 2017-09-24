#lang racket
(require "chapter-01.rkt")
(module+ test (require rackunit))


;; lat? :: List -> Bool
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

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



;; member? :: Atom -> List Atom -> List Atom
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? a (car lat))
                (member? a (cdr lat)))))))

(module+ test
  (check-true
    (member? 'tea '(coffee tea or milk))
    "Tea is present in the list")

  (check-false
    (member? 'poached '(fried eggs and scrambledd eggs))
    "poached is not present in the list"))


