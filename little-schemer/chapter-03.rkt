#lang racket
(require "chapter-01.rkt")
(module+ test (require rackunit))


;; rember :: Atom -> List Atom -> List Atom
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (rember a (cdr lat)))
      (else (cons (car lat) (rember a (cdr lat)))))))

(module+ test
  (check-equal?
    (rember 2 '(1 2 3)) '(1 3)
    "'(1 2 3) without 2 is '(1 3)")

  (check-equal?
    (rember 2 '()) '()
    "Removing 2 out of an empty list is a empty list")

  (check-equal?
    (rember 2 '(5 6 7)) '(5 6 7)
    "If element not on a list the list stays unchanged"))



;; firsts :: List -> List
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cons (car l) (firsts (cdr l))))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(module+ test
  (check-equal?
   (firsts '((apple peach pumpkin)
             (plum pear cherry)
             (grape raisin pea)
             (bean carrot oggplant)))
   '(apple plum grape bean)
   "Collect first elements from a list of lists")

  (check-equal?
   (firsts '((a b) (c d) (e f))) '(a c e)
   "Collect first elements from a list of lists")

  (check-equal?
   (firsts '((five plums)
             (four)
             (eleven green oranges)))
   '(five four eleven)
   "Collect first elements from a list of lists")

  (check-equal?
   (firsts '(((five plums))
             (eleven green oranges)
             ((no) more)))
   '((five plums) eleven (no))
   "Collect first elements from a list of lists"))



;; insertR :: Atom -> Atom -> List Atom -> List Atom
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(module+ test
  (check-equal?
   (insertR 'topping 'fudge
            '(ice cream with fudge for dessert))
   '(ice cream with fudge topping for dessert)
   "Insert an element to the right of already present element")

  (check-equal?
    (insertR 'jalapeno 'and
             '(tacos tamales and salsa))
    '(tacos tamales and jalapeno salsa)
    "Insert an element to the right of already present element")

  (check-equal?
    (insertR 'e 'd '(a b c d f g h))
    '(a b c d e f g h)
    "Insert an element to the right of already present element")

  (check-equal?
    (insertR 'x 'y '(a b c))
    '(a b c)
    "Return the list untouched when old element not present"))


;; insertL :: Atom -> Atom -> List Atom -> List Atom
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new (cons (car lat) (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(module+ test
  (check-equal?
   (insertL 'topping 'for
            '(ice cream with fudge for dessert))
   '(ice cream with fudge topping for dessert)
   "Insert an element to the right of already present element")

  (check-equal?
   (insertL 'jalapeno 'salsa
            '(tacos tamales and salsa))
   '(tacos tamales and jalapeno salsa)
   "Insert an element to the right of already present element")

  (check-equal?
   (insertL 'e 'f
            '(a b c d f g h))
   '(a b c d e f g h)
   "Insert an element to the right of already present element")

  (check-equal?
   (insertL 'x 'y '(a b c))
   '(a b c)
   "Return the list untouched when old element not present"))


;; subst :: Atom -> Atom -> List Atom -> List Atom
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(module+ test
  (check-equal?
   (subst 'topping 'fudge
          '(ice cream with fudge for dessert))
   '(ice cream with topping for dessert)
   "Replace 'fudge with a 'topping")

  (check-equal?
   (subst 'topping 'plumns
          '(ice cream with fudge for dessert))
   '(ice cream with fudge for dessert)
   "Return the list untouched when old element not present"))


