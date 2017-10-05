#lang racket
(module+ test (require rackunit))
(require "chapter-01.rkt"
         "chapter-02.rkt"
         "chapter-03.rkt")
(provide build first second)


;; set? :: List -> Bool
(module+ test
  (check-false
    (set? '(apple peaches apple plum))
    "Dublicated apple makes it no set")

  (check-true
    (set? '(apple peaches pear plum))
    "All elements are distinct so it's a set"))

(define set?
  (λ (l)
     (cond
       ((null? l) #t)
       ((member? (car l) (cdr l)) #f)
       (else (set? (cdr l))))))



;; makeset :: List -> Set
(module+ test
  (check-equal?
    (makeset '(apple peach pear peach
               plum apple lemon peach))
    '(apple peach pear plum lemon)
    "Apple and peach duplicates removed to make a set"))

(define makeset
  (λ (l)
     (cond
       ((null? l) '())
       (else
         (cons (car l)
               (makeset (multirember (car l) (cdr l))))))))



;; subset? :: Set -> Set -> Bool
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

(define subset?
  (λ (s1 s2)
     (cond
       ((null? s1) #t)
       (else
         (and (member? (car s1) s2)
              (subset? (cdr s1) s2))))))


;; eqset? :: Set -> Set -> Bool
(module+ test
  (check-true
    (eqset? '(6 large chickens with wings)
            '(6 chickens with large wings))
    "Both sets are the same"))

(define eqset?
  (λ (s1 s2)
     (and (subset? s1 s2)
          (subset? s2 s1))))


;; intersect? :: Set -> Set -> Bool
(module+ test
  (check-true
    (intersect? '(stewed tomatoes and maacaroni)
                '(macaroni and cheese))
    "Word 'and' appears in both sets"))

(define intersect?
  (λ (s1 s2)
     (cond
       ((null? s1) #f)
       (else
         (or (member? (car s1) s2)
             (intersect? (cdr s1) s2))))))



;; intersect :: Set -> Set -> Set
(module+ test
  (check-equal?
    (intersect '(stewed tomatoes and macaroni)
               '(macaroni and cheese))
    '(and macaroni)))

(define intersect
  (λ (s1 s2)
     (cond
       ((null? s1) '())
       ((member? (car s1) s2)
        (cons (car s1) (intersect (cdr s1) s2)))
       (else
         (intersect (cdr s1) s2)))))



;; union :: Set -> Set -> Set
(module+ test
  (check-equal?
    (union '(stewed tomatoes and macaroni casserole)
           '(macaroni and cheese))
    '(stewed tomatoes casserole macaroni and cheese)))

(define union
  (λ (s1 s2)
     (cond
      ((null? s1) s2)
      ((member? (car s1) s2)
       (union (cdr s1) s2))
      (else
        (cons (car s1) (union (cdr s1) s2))))))



;; intersectall :: List Set -> Set
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

(define intersectall
  (λ (l-set)
     (cond
       ((null? (cdr l-set)) (car l-set))
       (else
         (intersect (car l-set)
                    (intersectall (cdr l-set)))))))



;; a-pair? :: List -> Bool
(module+ test
  (check-true
    (a-pair? '(pear pear)))

  (check-true
    (a-pair? '(3 7)))

  (check-true
    (a-pair? '((2) (pair))))

  (check-true
    (a-pair? '(full (house)))))

(define a-pair?
  (λ (l)
     (cond
       ((null? l) #f)
       ((atom? l) #f)
       ((null? (car l)) #f)
       ((null? (cdr (cdr l))) #t)
       (else #f))))



;; first :: Pair -> S-Exp
(module+ test
  (check-equal?
    (first '((2) (pair)))
    '(2)
    "First in ((2) (pair)) is (2)"))

(define first
  (λ (pair)
     (car pair)))



;; second :: Pair -> S-Exp
(module+ test
  (check-equal?
    (second '((2) (pair)))
    '(pair)
    "Second in ((2) (pair)) is (pair)"))

(define second
  (λ (pair)
     (car (cdr pair))))



;; build :: S-Exp -> S-Exp -> Pair
(module+ test
  (check-equal?
    (build '(2) '(pair))
    '((2) (pair))
    "Pair of (2) and (pair) is ((2) (pair))"))

(define build
  (λ (s1 s2)
     (cons s1 (cons s2 '()))))


;; third :: Pair -> S-Exp
(module+ test
  (check-equal?
    (third '((one) zwei ((tri))))
    '((tri))
    "third in ((one) zwei ((tri))) is ((tri))"))

(define third
  (λ (l)
     (car (cdr (cdr l)))))



;; fun? :: List Rel -> Bool
(module+ test
  (check-true
    (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
    "All firsts are unique")

  (check-false
    (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
    "b is duplicated among the firsts"))

(define fun?
  (λ (l-rel)
     (set? (firsts l-rel))))



;; revpair :: Pair -> Pair
(module+ test
  (check-equal?
    (revpair '(1 (2)))
    '((2) 1)
    "Pair is reversed"))

(define revpair
  (λ (pair)
     (build (second pair)
            (first pair))))



;; revrel :: List Rel -> List Rel
(module+ test
  (check-equal?
    (revrel '((8 a) (pumpkin pie) (got sick)))
    '((a 8) (pie pumpkin) (sick got))
    "All pairs on the list has been reversed"))

(define revrel
  (λ (rel)
     (cond
       ((null? rel) '())
       (else
         (cons (revpair (car rel))
               (revrel (cdr rel)))))))



;; seconds :: List Pair -> List
(module+ test
  (check-equal?
    (seconds '((8 3) (4 2) (7 6) (6 2) (3 4)))
    '(3 2 6 2 4)
    "All second S-Expressions from the list of pairs")

  (check-equal?
    (seconds '((d 4) (b 0) (b 9) (e 5) (g 4)))
    '(4 0 9 5 4)
    "All second S-Expressions from the list of pairs"))

(define seconds
  (λ (l-pair)
     (cond
       ((null? l-pair) '())
       (else
         (cons (second (car l-pair))
               (seconds (cdr l-pair)))))))



;; fullfun? :: Fun -> Bool
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

(define fullfun?
  (λ (l-rel)
     (set? (seconds l-rel))))



;; one-to-one? :: Fun -> Bool
(module+ test
  (check-true
    (one-to-one? '((chocolate chip) (doughty cookie)))
    "It's a tasty one-to-one function"))

(define one-to-one?
  (λ (fun)
     (fun? (revrel fun))))

