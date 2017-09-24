#lang racket
(require "chapter-01.rkt")
(module+ test (require rackunit))


;; rember :: Atom -> List Atom -> List Atom
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(module+ test
  (check-equal?
    (rember 'mint '(lamb chops and mint jelly))
    '(lamb chops and jelly)
    "An element removed from list")

  (check-equal?
    (rember 'mint '(lamb chops and mint flavored mint jelly))
    '(lamb chops and flavored mint jelly)
    "Only first found element removed from list")

  (check-equal?
    (rember 'toast '(bacon lettuce and tomato))
   '(bacon lettuce and tomato)
    "List returned untouched when element not found")

  (check-equal?
    (rember 'cup '(coffee cup tea cup and hick cup))
    '(coffee tea cup and hick cup)
    "Only first found element removed from list"))



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



;; subst2 :: Atom -> Atom -> Atom -> List Atom -> List Atom
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (equal? (car lat) o1)
           (equal? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(module+ test
  (check-equal?
    (subst2 'vanilla
            'chocolate 'banana
            '(banana ice cream with chocolate topping))
    '(vanilla ice cream with chocolate topping)
    "Should replace one of the old elements")

  (check-equal?
    (subst2 'vanilla
            'chocolate 'banana
            '(lemon ice cream with chocolate topping))
    '(lemon ice cream with vanilla topping)
    "Should replace one of the old elements even if the other is not present"))



;; multirember :: Atom -> List Atom -> List Atom
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(module+ test
  (check-equal?
    (multirember 'cup '(coffee cup tea cup and hick cup))
    '(coffee tea and hick)
    "Should remove all occurences of an atom from the list of atoms"))



;; multiinsertR :: Atom -> Atom -> List Atom -> List Atom
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old)
       (cons (car lat)
             (cons new (multiinsertR new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertR new old (cdr lat)))))))

(module+ test
  (check-equal?
   (multiinsertR 'lemon 'ice
                 '(ice cream with ice for dessert))
   '(ice lemon cream with ice lemon for dessert)
   "Insert an element to the right of already present elements"))



;; multiinsertL :: Atom -> Atom -> List Atom -> List Atom
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old)
       (cons new
             (cons (car lat) (multiinsertL new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertL new old (cdr lat)))))))

(module+ test
  (check-equal?
   (multiinsertL 'lemon 'ice
                 '(ice cream with ice for dessert))
   '(lemon ice cream with lemon ice for dessert)
   "Insert an element to the right of already present elements"))



;; multisubst :: Atom -> Atom -> List Atom -> List Atom
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) old)
       (cons new (multisubst new old (cdr lat))))
      (else
        (cons (car lat) (multisubst new old (cdr lat)))))))

(module+ test
  (check-equal?
   (multisubst 'lemon 'ice
               '(ice cream with ice for dessert))
   '(lemon cream with lemon for dessert)
   "Insert an element to the right of already present elements"))

