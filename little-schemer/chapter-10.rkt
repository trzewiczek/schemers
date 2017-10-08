#lang racket
(module+ test (require rackunit))
(require "chapter-01.rkt"
         "chapter-07.rkt")

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



;; expression-to-action
(module+ test)

(define expression-to-action
  (λ (e)
     (cond
       ((atom? e) (atom-to-action))
       (else (list-to-action)))))

(define atom-to-action
  (λ (e)
     (cond
       ((number? e) *const)
       ((eq? e #t) *const)
       ((eq? e #f) *const)
       ((eq? e 'cons) *const)
       ((eq? e 'car) *const)
       ((eq? e 'cdr) *const)
       ((eq? e 'null?) *const)
       ((eq? e 'eq?) *const)
       ((eq? e 'atom?) *const)
       ((eq? e 'zero?) *const)
       ((eq? e 'add1) *const)
       ((eq? e 'sub1) *const)
       ((eq? e 'number?) *const)
       (else *identifier))))

(define list-to-action
  (λ (e)
     (cond
       ((atom? (car e))
        (cond
          ((eq? (car e) 'quote) *quote)
          ((eq? (car e) 'lambda) *lambda)
          ((eq? (car e) 'cond) *cond)
          (else *application)))
       (else *application))))

(define value
  (λ (e)
     (meaning e '())))

(define meaning
  (λ (e table)
     ((expression-to-action e) e table)))

(define *const
  (λ (e table)
     (cond
       ((number? e) e)
       ((eq? e #t) #t)
       ((eq? e #f) #f)
       (else (build 'primitive e)))))

(define *quote
  (λ (e table)
     (text-of e)))

(define text-of second)

(define *identifier
  (λ (e table)
     (lookup-in-table e initial-table)))

(define initial-table
  (λ (name)
     (car '())))

(define *lambda
  (λ (e table)
     (build 'non-primitive (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (λ (lines table)
     (cond
       ((else? (question-of (car lines)))
        (meaning (answer-of (car lines)) table))
       ((meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table))
       (else
         (evcon (cdr lines) table)))))

(define else?
  (λ (e)
     (cond
       ((atom? e) (eq? e 'else))
       (else #f))))

(define question-of first)

(define answer-of second)

(define *cond
  (λ (e table)
     (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlist
  (λ (args table)
    (cond
      ((null? args) '())
      (else
        (cons (meaning (car args))
              (evlist (cdr args) table))))))
