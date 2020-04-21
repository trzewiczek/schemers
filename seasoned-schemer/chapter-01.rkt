#lang racket
(module+ test (require rackunit))
(provide member?)


;; member? :: Atom -> List<Atom> -> Bool
(module+ test
   (check-true
     (member? 'tea '(coffee tea or milk))
     "tea is present in the list")

   (check-false
     (member? 'poached '(fried eggs and scrambledd eggs))
     "poached is not present in the list"))

(define member?
  (λ (a lat)
     (cond
       ((null? lat) #f)
       (else (or (eq? (car lat) a)
                 (member? a (cdr lat)))))))


;; is-first? :: Atom -> List<Atom> -> Bool
(module+ test
   (check-true
     (is-first? 'tea '(tea coffe or milk))
     "tea is the first one on the list")

   (check-false
     (is-first? 'tea '(coffee tea or milk))
     "tea is on the list, but not the first one"))

(define is-first?
  (λ (a lat)
     (cond
       ((null? lat) #f)
       (else (eq? (car lat) a)))))


;; two-in-a-row-b? :: Atom -> List<Atom> -> Bool
(module+ test
   (check-true
     (two-in-a-row-b? 'tea '(tea coffe or milk))
     "tea is the first one on the list")

   (check-false
     (two-in-a-row-b? 'tea '(coffee tea or milk))
     "tea is on the list, but not the first one"))

(define two-in-a-row-b?
  (λ (preceding lat)
     (cond
       ((null? lat) #f)
       (else (or (eq? preceding (car lat))
                 (two-in-a-row-b? (car lat) (cdr lat)))))))

;; two-in-a-row? :: List<Atom> -> Bool
(module+ test
   (check-true
     (two-in-a-row? '(Italian sardines sardines spaghetti))
     "sardines is twice in a row")

   (check-false
     (two-in-a-row? '(Italian sardines spaghetti sardines))
     "sardines twice, but not in a row"))

(define two-in-a-row?
  (λ (lat)
     (cond
       ((null? lat) #f)
       (else (two-in-a-row-b? (car lat) (cdr lat))))))


;; sum-of-prefixes :: Tuple -> Number
(module+ test
   (check-equal?
     (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5)
     "'(1 1 1 1 1) -> '(1 2 3 4 5)")

   (check-equal?
     (sum-of-prefixes '(1 2 3 4 5)) '(1 3 6 10 15)
     "(1 2 3 4 5) -> '(1 3 6 10 15)"))

(define sum-of-prefixes
  (λ (tup)
     (sum-of-prefixes-b 0 tup)))

;; sum-of-prefixes-b :: Tuple -> Number
(module+ test
   (check-equal?
     (sum-of-prefixes-b 0 '(1 1 1)) '(1 2 3)
     "0 -> '(1 1 1) -> '(1 2 3)")

   (check-equal?
     (sum-of-prefixes-b 3 '(1 2 3)) '(4 6 9)
     "3 -> '(1 2 3) -> '(4 6 9"))

(define sum-of-prefixes-b
  (λ (acc tup)
     (cond
       ((null? tup) '())
       (else (cons
               (+ acc (car tup))
               (sum-of-prefixes-b (+ acc (car tup))
                                  (cdr tup)))))))

;; pick :: Atom -> List<Atom> -> Atom
(module+ test
   (check-equal?
     (pick 4 '(4 3 1 1 1)) 1
     "4 -> '(4 3 1 1 1) -> 1")

   (check-equal?
     (pick 2 '(2 4 3 1 1 1)) 4
     "2 -> '(2 4 3 1 1 1) -> 4"))

(define pick
  (λ (n lat)
     (cond
       ((null? lat) #f)
       ((eq? n 1) (car lat))
       (else (pick (sub1 n) (cdr lat))))))
