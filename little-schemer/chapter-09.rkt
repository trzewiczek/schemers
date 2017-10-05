#lang racket
(module+ test (require rackunit))
(require "chapter-01.rkt"
         "chapter-04.rkt"
         "chapter-07.rkt")

;; looking :: Atom -> List Atom -> Bool
(module+ test
  (check-true
    (looking 'caviar '(6 2 4 caviar 5 7 3))
    "Found caviar. Tasty!")

  (check-false
    (looking 'caviar '(6 2 grits caviar 5 7 3))
    "Couldn't find caviar, so brought this..."))

(define looking
  (λ (a lat)
     (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (λ (a sorn lat)
    (cond
      ((number? sorn)
        (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))



;; shift :: Pair -> Pair
(module+ test
  (check-equal?
    (shift '((a b) c)) '(a (b c))
    "Pair and lonely -> Lonely and a pair")

  (check-equal?
    (shift '((a b) (c d))) '(a (b (c d)))
    "Pair of pairs -> consed list"))

(define shift
  (λ (pair)
     (build (first (first pair))
            (build (second (first pair))
                   (second pair)))))



;; Y-Combinator :: Fun -> Fun
(module+ test
  (check-equal?
    (len '()) 0
    "Length of empty list is 0!")

  (check-equal?
    (len '(1 2 3)) 3
    "Three items on the list, so legnth equals 3"))

(define Y
  (λ (le)
     ((λ (f) (f f))
      (λ (f)
         (le (λ (x) ((f f) x)))))))

(define len
  (Y (λ (cont)
        (λ (l)
           (cond
             ((null? l) 0)
             (else
               (add1 (cont (cdr l)))))))))
