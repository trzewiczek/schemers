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



;; align :: Pair -> Pair
(module+ test
  (check-equal?
    (shift '((a b) c)) '(a (b c))
    "Pair and lonely -> Lonely and a pair"))

(define align
  (λ (pora)
     (cond
       ((atom? pora) pora)
       ((pair? (first pora))
        (align (shift pora)))
       (else
         (build (first pora)
                (align (second pora)))))))


;; length* :: Pair -> Number
(module+ test
  (check-equal?
    (length* '(((a b) (c (d e))) (f (g h)))) 8
    "It's 8 atoms in these pars of pairs of pairs..."))

(define length*
  (λ (pora)
     (cond
       ((atom? pora) 1)
       (else
         (+ (length* (first pora))
            (length* (second pora)))))))


;; weight* ;; Pair -> Number
(module+ test
  (check-equal?
    (weight* '((a b) c)) 7
    "The leftmost part weights a lot...")

  (check-equal?
    (weight* '(a (b c))) 5
    "The leftmost part is light like a gain of sand."))

(define weight*
  (λ (pora)
     (cond
       ((atom? pora) 1)
       (else
         (+ (* (weight* (first pora)) 2)
            (weight* (second pora)))))))



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
