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
    (lat? '())
    "Empty list is a list of atoms")

  (check-true
    (lat? '(1 2 "ladida"))
    "List of numbers and strings is a list of atoms")

  (check-false
    (lat? '('()))
    "A list of empty list is not a list of atoms")

  (check-false
    (lat? '(1 2 ('ladida) "ladida"))
    "A list containing a list is not a list of atoms"))



;; member? :: Atom -> List Atom -> List Atom
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? a (car lat))
                (member? a (cdr lat)))))))

(module+ test
  (check-true
    (member? 2 '(1 2 3))
    "2 is a member of list '(1 2 3)")

  (check-true
    (member? 2 '(1 2 3 2))
    "2 is a member of list '(1 2 3 2)")

  (check-false
    (member? 2 '(1 3))
    "2 is not a member of list '(1 3)")

  (check-false
    (member? 2 '(1 '(2) 3))
    "2 is not a member of list '(1 '(2) 3)"))


