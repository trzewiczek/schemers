#lang racket
(module+ test (require rackunit))
(require "chapter-01.rkt"
         "chapter-04.rkt"
         "chapter-06.rkt")


;; rember-f :: (a -> b -> Bool) -> a -> List a -> List a
(module+ test
  (check-equal?
    ((rember-f =) 5 '(6 2 5 3))
    '(6 2 3)
    "Five is removed with = test used")

  (check-equal?
    ((rember-f eq?) 'jelly '(jelly beans are good))
    '(beans are good)
    "Jelly removed from the tasty menu position")

  (check-equal?
    ((rember-f equal?)
              '(pop corn)
              '(lemonade (pop corn) and (cake)))
    '(lemonade and (cake))
    "(pop corn) removed from the tasty menu position"))

(define rember-f
  (λ (test?)
     (λ (a l)
        (cond
          ((null? l) '())
          ((test? a (car l))
           ((rember-f test?) a (cdr l)))
          (else
            (cons (car l)
                  ((rember-f test?) a (cdr l))))))))


;; insertL-f :: (a -> a -> Bool) -> a -> a -> List a -> List a
(module+ test
  (check-equal?
    ((insertL-f =) 1 5 '(6 2 5 3))
    '(6 2 1 5 3)
    "One comes before five")

  (check-equal?
    ((insertL-f eq?) 'extra 'jelly '(jelly beans are good))
    '(extra jelly beans are good)
    "Jelly becomes extra")

  (check-equal?
    ((insertL-f equal?)
              'with
              '(pop corn)
              '(lemonade (pop corn) and (cake)))
    '(lemonade with (pop corn) and (cake))
    "(pop corn) preserved with 'with'"))

(define insertL-f
  (λ (test?)
     (λ (new old l)
        (cond
          ((null? l) '())
          ((test? old (car l))
           (cons new (cons old (cdr l))))
          (else (cons (car l)
                      ((insertL-f test?)
                       new old (cdr l))))))))


;; insertR-f :: (a -> a -> Bool) -> a -> a -> List a -> List a
(module+ test
  (check-equal?
    ((insertR-f =) 1 5 '(6 2 5 3))
    '(6 2 5 1 3)
    "One comes after five")

  (check-equal?
    ((insertR-f eq?) 'extra 'jelly '(jelly beans are good))
    '(jelly extra beans are good)
    "Beans becomes extra")

  (check-equal?
    ((insertR-f equal?)
              'pie
              '(pop corn)
              '(lemonade (pop corn) and (cake)))
    '(lemonade (pop corn) pie and (cake))
    "pie put on the list"))

(define insertR-f
  (λ (test?)
     (λ (new old l)
        (cond
          ((null? l) '())
          ((test? old (car l))
           (cons old (cons new (cdr l))))
          (else (cons (car l)
                      ((insertR-f test?)
                       new old (cdr l))))))))


;; seqL :: a -> a -> List a -> List a
(module+ test
  (check-equal?
    (seqL 'a 'b '(c d e))
    '(a b c d e)
    "a put before b and consed on c d e"))

(define seqL
  (λ (new old l)
     (cons new (cons old l))))


;; seqR :: a -> a -> List a -> List a
(module+ test
  (check-equal?
    (seqR 'b 'a '(c d e))
    '(a b c d e)
    "b put after a and consed on c d e"))

(define seqR
  (λ (new old l)
     (cons old (cons new l))))


;; insert-g :: (a -> a -> List a) -> a -> a -> List a
(module+ test
  (check-equal?
    ((insert-g seqR) 'extra 'jelly '(jelly beans are good))
    '(jelly extra beans are good)
    "Beans becomes extra with seqR")

  (check-equal?
    ((insert-g seqL) 'extra 'jelly '(jelly beans are good))
    '(extra jelly beans are good)
    "Jelly becomes extra with seqL"))

(define insert-g
  (λ (seq)
     (λ (new old l)
        (cond
          ((null? l) '())
          ((equal? old (car l))
           (seq new old (cdr l)))
          (else
            (cons (car l)
                  ((insert-g seq) new old (cdr l))))))))


;; seqS :: a -> a -> List a -> List a
(module+ test
  (check-equal?
    (seqS 'a 'x '(b c d))
    '(a b c d)
    "a consed on b c d"))

(define seqS
  (λ (new old l)
     (cons new l)))


;; subst :: a -> a -> List a -> List a
(module+ test
  (check-equal?
    (subst 'extra 'jelly '(jelly beans are good))
    '(extra beans are good)
    "Extrra takes place of jelly"))

(define subst
  (insert-g seqS))


;; atom-to-function :: Atom -> (Number -> Number -> Number)
(module+ test
  (check-equal?
    (atom-to-function '+) o+
    "+ resolves to addition function")

  (check-equal?
    (atom-to-function '*) o*
    "* resolves to multiplication function")

  (check-equal?
    (atom-to-function '^) o^
    "^ resolves to power function"))

(define atom-to-function
  (λ (a)
     (cond
       ((eq? a '+) o+)
       ((eq? a '*) o*)
       (else o^))))


;; value :: N-Exp -> Number
(module+ test
  (check-equal?
    (value '(+ 5 3)) 8
    "5 + 3 -> 8")

  (check-equal?
    (value '(+ 5 (* 3 2))) 11
    "5 + (3 * 2) -> 11")

  (check-equal?
    (value '(+ 5 (* 3 (^ 2 2)))) 17
    "5 + (3 * (2 ^ 2)) -> 17"))

(define value
  (λ (nexp)
     (cond
       ((atom? nexp) nexp)
       (else
         ((atom-to-function (operator nexp))
          (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp)))))))


;; multirember-f :: (a -> a -> Bool) -> a -> List a -> List a
(module+ test
  (check-equal?
    ((multirember-f eq?) 'tuna
                         '(shrimp salad tuna salad and tuna))
    '(shrimp salad salad and)
    "All tuna removed from the dinner"))

(define multirember-f
  (λ (test?)
     (λ (a l)
        (cond
          ((null? l) '())
          ((test? a (car l))
           ((multirember-f test?) a (cdr l)))
          (else
            (cons (car l)
                  ((multirember-f test?) a (cdr l))))))))


;; last-friend :: List a -> List a -> Number
(module+ test
  (check-equal?
    (last-friend '(strawberries and swordfish) '(tuna)) 3
    "Three words in this dish"))

(define last-friend
  (λ (l1 l2)
     (length l1)))


;; multirember&co :: a -> List a -> (List a -> List a -> b) -> b
(module+ test
  (check-equal?
    (multirember&co 'tuna
                    '(strawberries tuna and swordfish)
                    last-friend)
    3
    "Three words that are not tuna"))

(define multirember&co
  (λ (a lat col)
     (cond
       ((null? lat) (col '() '()))
       ((eq? (car lat) a)
        (multirember&co a (cdr lat)
                        (λ (newlat seen)
                           (col newlat
                                (cons (car lat) seen)))))
       (else
         (multirember&co a (cdr lat)
                         (λ (newlat seen)
                            (col (cons (car lat) newlat)
                                 seen)))))))


;; multiinertLR :: a -> a -> a -> List a -> List a
(module+ test
  (check-equal?
    (multiinertLR 'jelly 'tuna 'and
                    '(strawberries tuna and swordfish))
    '(strawberries jelly tuna and jelly swordfish)
    "Jelly here, jelly there..."))

(define multiinertLR
  (λ (new oldL oldR lat)
     (cond
       ((null? lat) '())
       ((eq? oldL (car lat))
        (cons new
              (cons oldL
                    (multiinertLR new oldL oldR (cdr lat)))))

       ((eq? oldR (car lat))
        (cons oldR
              (cons new
                    (multiinertLR new oldR oldR (cdr lat)))))
       (else
         (cons (car lat)
               (multiinertLR new oldL oldR (cdr lat)))))))


;; multiinertLR&co :: a -> a -> a -> List a -> (List a -> Number -> Number -> b) -> b
(module+ test
  (check-equal?
    (multiinertLR&co 'salty 'fish 'chips
                     '(chips and fish or fish and chips)
                     (λ (newlat L R) newlat))
    '(chips salty and salty fish or
      salty fish and chips salty)
    "Collector return a multiinserted list")

  (check-equal?
    (multiinertLR&co 'salty 'fish 'chips
                     '(chips and fish or fish and chips)
                     (λ (newlat L R) L))
    2
    "Two inserts on the left")

  (check-equal?
    (multiinertLR&co 'salty 'fish 'chips
                     '(chips and fish or fish and chips)
                     (λ (newlat L R) R))
    2
    "Two inserts on the right"))

(define multiinertLR&co
  (λ (new oldL oldR lat col)
     (cond
       ((null? lat) (col '() 0 0))
       ((eq? (car lat) oldL)
        (multiinertLR&co new oldL oldR (cdr lat)
                         (λ (newlat L R)
                            (col (cons new (cons oldL newlat))
                                 (add1 L)
                                 R))))
       ((eq? (car lat) oldR)
        (multiinertLR&co new oldL oldR (cdr lat)
                         (λ (newlat L R)
                            (col (cons oldR (cons new newlat))
                                 L
                                 (add1 R)))))
       (else
         (multiinertLR&co new oldL oldR (cdr lat)
                          (λ (newlat L R)
                             (col (cons (car lat) newlat)
                                  L
                                  R)))))))


;; even? :: Number -> Bool
(module+ test
  (check-true
    (even? 2)
    "Two is even enough")

  (check-false
    (even? 3)
    "Three is too less of an even"))

(define even
  (λ (n)
    (= (* (/ n 2) 2) n)))



;; evens-only* :: List -> List
(module+ test
  (check-equal?
    (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
    '((2 8) 10 (() 6) 2)
    "Some places let so empty without these odd ones"))

(define evens-only*
  (λ (l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((even? (car l)) (cons (car l)
                                 (evens-only* (cdr l))))
          (else
            (evens-only* (cdr l)))))
       (else
         (cons (evens-only* (car l))
               (evens-only* (cdr l)))))))



;; evens-only*&co :: List -> (List -> Number -> Number -> a) -> a
(module+ test
  (check-equal?
    (evens-only*&co
      '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
      (λ (newl S M) (cons S (cons M newl))))
    '(38 1920 (2 8) 10 (() 6) 2)
    "Collector did its job!"))

(define evens-only*&co
  (λ (l col)
     (cond
       ((null? l) (col '() 0 1))
       ((atom? (car l))
        (cond
          ((even? (car l))
           (evens-only*&co (cdr l)
                           (λ (newl S M)
                              (col (cons (car l) newl)
                                   S
                                   (* (car l) M)))))
          (else
            (evens-only*&co (cdr l)
                            (λ (newl S M)
                               (col newl
                                    (+ (car l) S)
                                    M))))))
       (else
         (evens-only*&co (car l)
           (λ (h-newl h-S h-M)
              (evens-only*&co (cdr l)
                (λ (r-newl r-S r-M)
                   (col (cons h-newl r-newl)
                        (+ h-S r-S)
                        (* h-M r-M))))))))))

