#lang racket
(module+ test (require rackunit))
(provide add1 eqan? o+ o* o^)


;; add1 :: Number -> Number
(define add1
  (λ (n)
    (+ n 1)))

(module+ test
  (check-equal?
    (add1 67) 68
    "It should add 1 to number n"))



;; sub1 :: Number -> Number
(define sub1
  (λ (n)
    (- n 1)))

(module+ test
  (check-equal?
    (sub1 5) 4
    "It should substitute 1 from number n"))



;; o+ :: Number -> Number -> Number
(define o+
  (λ (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(module+ test
  (check-equal?
    (o+ 46 12) 58
    "Should add number n to number m"))



;; o- :: Number -> Number -> Number
(define o-
  (λ (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(module+ test
  (check-equal?
    (o- 14 3) 11
    "Should add number n to number m")

  (check-equal?
    (o- 17 9) 8
    "Should add number n to number m"))



;; addtup :: Tuple -> Number
(define addtup
  (λ (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup)
                (addtup (cdr tup)))))))

(module+ test
  (check-equal?
    (addtup '(3 5 2 8)) 18
    "Should sum up the tuple")

  (check-equal?
    (addtup '(15 6 7 12 3)) 43
    "Should sum up the tuple"))



;; o* :: Number -> Number -> Number
(define o*
  (λ (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

(module+ test
  (check-equal?
    (o* 5 3) 15
    "Should multiply n times m")

  (check-equal?
    (o* 13 4) 52
    "Should multiply n times m"))



;; tup+ :: Tuple -> Tuple -> Tuple
(define tup+
  (λ (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (o+ (car tup1) (car tup2))
              (tup+ (cdr tup1) (cdr tup2)))))))

(module+ test
  (check-equal?
    (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
    '(11 11 11 11 11)
    "Should add each number from tup1 to corresponding element in tup2")

  (check-equal?
    (tup+ '(2 3) '(4 6))
    '(6 9)
    "Should add each number from tup1 to corresponding element in tup2")

  (check-equal?
    (tup+ '(3 7) '(4 6 8 1))
    '(7 13 8 1)
    "Should add each number from tup1 to corresponding element in tup2 when tup2 is longer")

  (check-equal?
    (tup+ '(3 7 4 6) '(8 1))
    '(11 8 4 6)
    "Should add each number from tup1 to corresponding element in tup2 when tup2 is shorter"))



;; o> :: Number -> Number -> Bool
(define o>
  (λ (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (o> (sub1 n) (sub1 m))))))

(module+ test
  (check-true
    (o> 13 4)
    "Should be true when n is greater then m")

  (check-false
    (o> 12 133)
    "Should be false when n is smaller then m")

  (check-false
    (o> 6 6)
    "Should be false when n is greater then m"))



;; o< :: Number -> Number -> Bool
(define o<
  (λ (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (o< (sub1 n) (sub1 m))))))

(module+ test
  (check-true
    (o< 4 6)
    "Should be true when n is smaller then m")

  (check-false
    (o< 8 3)
    "Should be false when n is greater then m")

  (check-false
    (o< 6 6)
    "Should be false when n is smaller then m"))



;; o= :: Number -> Number -> Bool
(define o=
  (λ (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

(module+ test
  (check-true
    (o= 6 6)
    "Equal numbers should be equal, shouldn't they?")

  (check-false
    (o= 6 7)
    "Different numbers should not be equal")

  (check-false
    (o= 7 6)
    "Different numbers should not be equal"))



;; o^ :: Number -> Number -> Number
(define o^
  (λ (n m)
    (cond
      ((zero? m) 1)
      (else
        (o* n (o^ n (sub1 m)))))))

(module+ test
  (check-equal?
    (o^ 1 1) 1
    "One to the power of one is one")

  (check-equal?
    (o^ 2 3) 8
    "Two to the power of three is eight")

  (check-equal?
    (o^ 5 3) 125
    "Five to the power of three is... 125"))



;; o/ :: Number -> Number -> Number
(define o/
  (λ (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (o/ (o- n m) m))))))

(module+ test
  (check-equal?
    (o/ 15 4) 3
    "Intereger division of 15 by 4 is 3"))



;; length :: List Atom -> Number
(define length
  (λ (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(module+ test
  (check-equal?
    (length '(hotdogs with mustard sauerkraut and pickles)) 6
    "This menu is 6 words long")

  (check-equal?
    (length '(ham and cheese on rye)) 5
    "This non vegan dish is 5 words long"))



;; pick :: Number -> List Atom -> Atom
(define pick
  (λ (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(module+ test
  (check-equal?
    (pick 4 '(lasagna spaghetti ravioli macaroni meatball))
    'macaroni
    "Macaroni is 4th on this italian menu"))



;; one? :: Number -> Bool
(define one?
  (λ (n)
     (= n 1)))



;; rempick :: Number -> List Atom -> Atom
(define rempick
  (λ (n lat)
    (cond
      ((null? lat) '())
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(module+ test
  (check-equal?
    (rempick 3 '(hotdogs with hot mustard))
    '(hotdogs with mustard)
    "Hot is third word so the mustard won't be hot")

  (check-equal?
    (rempick 5 '(hotdogs with hot mustard))
    '(hotdogs with hot mustard)
    "List is untouched is you try to rempick beyond the list"))


;; no-nums :: List Atom -> List Atom
(define no-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(module+ test
  (check-equal?
    (no-nums '(5 pears 6 prunes 9 dates))
    '(pears prunes dates)))



;; all-nums :: List Atom -> Tuple
(define all-nums
  (λ (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(module+ test
  (check-equal?
    (all-nums '(5 pears 6 prunes 9 dates))
    '(5 6 9)))



;; eqan? :: Atom -> Atom -> Bool
(define eqan?
  (λ (n m)
    (cond
      ((and (number? n) (number? m)) (= n m))
      ((or (number? n) (number? m)) #f)
      (else (eq? n m)))))

(module+ test
  (check-true
    (eqan? 1 1)
    "Should be equal")

  (check-true
    (eqan? 'pineapple 'pineapple)
    "Should be equal")

  (check-false
    (eqan? 'pineapple 'apple)
    "Should not be equal"))



;; occur :: Atom -> List Atom -> Number
(define occur
  (λ (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat))
       (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(module+ test
  (check-equal?
    (occur 'ice '(ice cream with ice for dessert)) 2
    "Ice occurs twice on this list")

  (check-equal?
    (occur 'lemon '(ice cream with ice for dessert)) 0
    "Lemon doesn't occur on this list at all"))

