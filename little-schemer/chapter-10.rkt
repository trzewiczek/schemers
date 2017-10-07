#lang racket
(module+ test (require rackunit))
(require "chapter-07.rkt")

;; lookup-in-entry :: Atom -> Entry -> (Atom -> a) -> Atom | a
(module+ test
  (check-equal?
    (lookup-in-entry 'entree '((appetizer entree beverage)
                               (food tastes good))
                     (λ (name) name))
    'tastes
    "Entree refers to tastes")

  (check-equal?
    (lookup-in-entry 'dessert '((appetizer entree beverage)
                                (food tastes good))
                     (λ (name) name))
    'dessert
    "Callback handles if entry not found"))

(define new-entry build)

(define lookup-in-entry-help
  (λ (name names values entry-f)
     (cond
       ((null? names) (entry-f name))
       ((eq? name (car names)) (car values))
       (else
         (lookup-in-entry-help
           name (cdr names) (cdr values) entry-f)))))

(define lookup-in-entry
  (λ (name entry entry-f)
     (lookup-in-entry-help
       name (first entry) (second entry) entry-f)))


;; lookup-in-table :: Atom -> Table -> (Atom -> a) -> Atom | a
(module+ test
  (check-equal?
    (lookup-in-table 'entree
                     '(((entree dessert)
                        (spaghetti spumoni))
                       ((appetizer entree beverage)
                        (food tastes good)))
                     (λ (name) name))
    'spaghetti
    "Spaghetti is the first value corresponding to name entree"))

(define extend-table cons)

(define lookup-in-table
  (λ (name table table-f)
     (cond
       ((null? table) (table-f name))
       (else
         (lookup-in-entry name (car table)
                          (λ (name)
                             (lookup-in-table
                               name
                               (cdr table)
                               table-f)))))))
