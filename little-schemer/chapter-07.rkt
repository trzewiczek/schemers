#lang racket
(module+ test (require rackunit))
(require "chapter-01.rkt"
         "chapter-02.rkt"
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



;; subset? :: Set -> Set -> Bool
(define subset?
  (λ (s1 s2)
     (cond
       ((null? s1) #t)
       (else
         (and (member? (car s1) s2)
              (subset? (cdr s1) s2))))))

(module+ test
  (check-true
    (subset? '(5 chicken wings)
             '(5 hamburgers
               2 pieces fried chicken and
               light duckling wings))
    "All elements of (5 chicken wings) appear in second set")

  (check-false
    (subset? '(4 pounds of horseradish)
             '(four pounds chicken and
               5 ounces horseradish))
    "Not all elements of the first set appear in the second one"))


;; eqset? :: Set -> Set -> Bool
(define eqset?
  (λ (s1 s2)
     (and (subset? s1 s2)
          (subset? s2 s1))))

(module+ test
  (check-true
    (eqset? '(6 large chickens with wings)
            '(6 chickens with large wings))
    "Both sets are the same"))


;; intersect? :: Set -> Set -> Bool
(define intersect?
  (λ (s1 s2)
     (cond
       ((null? s1) #f)
       (else
         (or (member? (car s1) s2)
             (intersect? (cdr s1) s2))))))

(module+ test
  (check-true
    (intersect? '(stewed tomatoes and maacaroni)
                '(macaroni and cheese))
    "Word 'and' appears in both sets"))



;; intersect :: Set -> Set -> Set
(define intersect
  (λ (s1 s2)
     (cond
       ((null? s1) '())
       ((member? (car s1) s2)
        (cons (car s1) (intersect (cdr s1) s2)))
       (else
         (intersect (cdr s1) s2)))))

(module+ test
  (check-equal?
    (intersect '(stewed tomatoes and macaroni)
               '(macaroni and cheese))
    '(and macaroni)))



;; union :: Set -> Set -> Set
(define union
  (λ (s1 s2)
     (cond
      ((null? s1) s2)
      ((member? (car s1) s2)
       (union (cdr s1) s2))
      (else
        (cons (car s1) (union (cdr s1) s2))))))

(module+ test
  (check-equal?
    (union '(stewed tomatoes and macaroni casserole)
           '(macaroni and cheese))
    '(stewed tomatoes casserole macaroni and cheese)))



;; intersectall :: List Set -> Set
(define intersectall
  (λ (l-set)
     (cond
       ((null? (cdr l-set)) (car l-set))
       (else
         (intersect (car l-set)
                    (intersectall (cdr l-set)))))))

(module+ test
  (check-equal?
    (intersectall '((a b c) (c a d e) (e f g h a b)))
    '(a))

  (check-equal?
    (intersectall '((6 pears and)
                    (3 peaches and 6 peppers)
                    (8 pears and 6 plums)
                    (and 6 prunes with some apples)))
    '(6 and)))



;; a-pair? :: List -> Bool
(define a-pair?
  (λ (l)
     (cond
       ((null? l) #f)
       ((atom? l) #f)
       ((null? (car l)) #f)
       ((null? (cdr (cdr l))) #t)
       (else #f))))

(module+ test
  (check-true
    (a-pair? '(pear pear)))

  (check-true
    (a-pair? '(3 7)))

  (check-true
    (a-pair? '((2) (pair))))

  (check-true
    (a-pair? '(full (house)))))



;; first :: Pair -> S-Exp
(define first
  (λ (pair)
     (car pair)))

(module+ test
  (check-equal?
    (first '((2) (pair)))
    '(2)
    "First in ((2) (pair)) is (2)"))



;; second :: Pair -> S-Exp
(define second
  (λ (pair)
     (car (cdr pair))))

(module+ test
  (check-equal?
    (second '((2) (pair)))
    '(pair)
    "Second in ((2) (pair)) is (pair)"))



;; build :: S-Exp -> S-Exp -> Pair
(define build
  (λ (s1 s2)
     (cons s1 (cons s2 '()))))

(module+ test
  (check-equal?
    (build '(2) '(pair))
    '((2) (pair))
    "Pair of (2) and (pair) is ((2) (pair))"))


;; third :: Pair -> S-Exp
(define third
  (λ (l)
     (car (cdr (cdr l)))))

(module+ test
  (check-equal?
    (third '((one) zwei ((tri))))
    '((tri))
    "third in ((one) zwei ((tri))) is ((tri))"))



;; fun? :: List Rel -> Bool
(define fun?
  (λ (l-rel)
     (set? (firsts l-rel))))

(module+ test
  (check-true
    (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
    "All firsts are unique")

  (check-false
    (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
    "b is duplicated among the firsts"))



;; revpair :: Pair -> Pair
(define revpair
  (λ (pair)
     (build (second pair)
            (first pair))))

(module+ test
  (check-equal?
    (revpair '(1 (2)))
    '((2) 1)
    "Pair is reversed"))



;; revrel :: List Rel -> List Rel
(define revrel
  (λ (rel)
     (cond
       ((null? rel) '())
       (else
         (cons (revpair (car rel))
               (revrel (cdr rel)))))))

(module+ test
  (check-equal?
    (revrel '((8 a) (pumpkin pie) (got sick)))
    '((a 8) (pie pumpkin) (sick got))
    "All pairs on the list has been reversed"))



;; seconds :: List Pair -> List
(define seconds
  (λ (l-pair)
     (cond
       ((null? l-pair) '())
       (else
         (cons (second (car l-pair))
               (seconds (cdr l-pair)))))))

(module+ test
  (check-equal?
    (seconds '((8 3) (4 2) (7 6) (6 2) (3 4)))
    '(3 2 6 2 4)
    "All second S-Expressions from the list of pairs")

  (check-equal?
    (seconds '((d 4) (b 0) (b 9) (e 5) (g 4)))
    '(4 0 9 5 4)
    "All second S-Expressions from the list of pairs"))



;; fullfun? :: Fun -> Bool
(define fullfun?
  (λ (l-rel)
     (set? (seconds l-rel))))

(module+ test
  (check-false
    (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
    "2 is duplicated among the second elemnts of the pairs")

  (check-true
    (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
    "All second numbers of pairs are unique")

  (check-false
    (fullfun? '((grape raisin)
                (plum prune)
                (stewed prune)))
    "Prune shows up twice on the list of second pair elements")

  (check-true
    (fullfun? '((grape raisin)
                (plum prune)
                (stewed grape)))
    "All second elements are unique"))



;; one-to-one? :: Fun -> Bool
(define one-to-one?
  (λ (fun)
     (fun? (revrel fun))))

(module+ test
  (check-true
    (one-to-one? '((chocolate chip) (doughty cookie)))
    "It's a tasty one-to-one function"))

