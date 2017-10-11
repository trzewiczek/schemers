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


(define expression-to-action
  (λ (e)
     (cond
       ((atom? e) (atom-to-action e))
       (else (list-to-action e)))))


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
     (lookup-in-table e table initial-table)))

(define initial-table
  (λ (name)
     (car '())))

;; *lambda
(module+ test
  (check-equal?
    (meaning '(lambda (x) (cons x y))
             '(((y z) ((8) 9))))
    '(non-primitive
       ((((y z) ((8) 9))) (x) (cons x y)))))

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

;; *cond
(module+ test
  (check-equal?
    (*cond '(cond (coffee klatsch) (else part))
           '(((coffee) (#t))
             ((klatsch party) (5 (6)))))
    5
    "coffee -> klatsch -> 5"))

(define *cond
  (λ (e table)
     (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlist
  (λ (args table)
    (cond
      ((null? args) '())
      (else
        (cons (meaning (car args) table)
              (evlist (cdr args) table))))))

(define *application
  (λ (e table)
     (apply
       (meaning (function-of e) table)
       (evlist (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (λ (l)
     (eq? (first l) 'primitive)))

(define non-primitive?
  (λ (l)
     (eq? (first l) 'non-primitive)))

(define apply
  (λ (fun vals)
     (cond
       ((primitive? fun)
        (apply-primitive (second fun) vals))
       ((non-primitive? fun)
        (apply-closure (second fun) vals)))))

(define apply-primitive
  (λ (name vals)
     (cond
       ((eq? name 'cons)
        (cons (first vals) (second vals)))
       ((eq? name 'car)
        (car (first vals)))
       ((eq? name 'cdr)
        (cdr (first vals)))
       ((eq? name 'null?)
        (null? (first vals)))
       ((eq? name 'eq?)
        (eq? (first vals)))
       ((eq? name 'atom?)
        (:atom? (first vals)))
       ((eq? name 'zero?)
        (zero? (first vals)))
       ((eq? name 'add1)
        (add1 (first vals)))
       ((eq? name 'sub1)
        (sub1 (first vals)))
       ((eq? name 'number?)
        (number? (first vals))))))

(define :atom?
  (λ (x)
     (cond
       ((atom? x) #t)
       ((null? x) #f)
       ((eq? (car x) 'primitive) #t)
       ((eq? (car x) 'non-primitive) #t)
       (else #f))))

(define apply-closure
  (λ (closure vals)
    (meaning (body-of closure)
             (extend-table
               (new-entry (formals-of closure) vals)
               (table-of closure)))))

(module+ test
  (check-equal?
    (apply-closure '((((u v w)
                       (1 2 3))
                      ((x y z)
                       (4 5 6)))
                     (x y)
                     (cons z x))
                   '((a b c) (d e f)))
    '(6 a b c)
    "It goes all the parsing, application and finally..."))

