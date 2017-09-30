#lang racket
(module+ test (require rackunit))
(require "chapter-01.rkt"
         "chapter-04.rkt")
(provide 1st-sub-exp 2nd-sub-exp operator)


;; numbered? :: AritmeticExpression -> Bool
(define numbered?
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(module+ test
  (check-true
   (numbered? 1)
   "1 is an aritmetic expression")

  (check-true
   (numbered? '(3 + 4 * 5))
   "3 + 4 * 5 is an aritmetic expression")

  (check-true
   (numbered? '(3 + (4 ^ 5)))
   "3 + 4 ^ 5 is an aritmetic expression")

  (check-false
   (numbered? '(2 * sauasage))
   "3 * sausage is not an aritmetic expression"))



;; 1st-sub-exp :: AritmeticExpression -> AritmeticExpression
(define 1st-sub-exp
  (λ (aexp)
     (car (cdr aexp))))

(module+ test
  (check-equal?
    (1st-sub-exp '(+ (* 2 5) (* 3 6)))
    '(* 2 5)
    "1st subexpression of + * 2 5 * 3 6 is * 2 5"))



;; 2nd-sub-exp :: AritmeticExpression -> AritmeticExpression
(define 2nd-sub-exp
  (λ (aexp)
     (car (cdr (cdr aexp)))))

(module+ test
  (check-equal?
    (2nd-sub-exp '(+ (* 2 5) (* 3 6)))
    '(* 3 6)
    "2nd subexpression of + * 2 5 * 3 6 is * 3 6"))



;; operator :: AritmeticExpression -> Atom
(define operator
  (λ (aexp)
     (car aexp)))

(module+ test
  (check-equal?
    (operator '(+ (* 2 5) (* 3 6)))
    '+
    "Operator of + * 2 5 * 3 6 is +"))



;; value :: AritmeticExpression -> Number
(define value
  (λ (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? (operator aexp) '+)
       (o+ (value (1st-sub-exp aexp))
           (value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) '*)
       (o* (value (1st-sub-exp aexp))
           (value (2nd-sub-exp aexp))))
      ((eq? (operator aexp) '^)
       (o^ (value (1st-sub-exp aexp))
           (value (2nd-sub-exp aexp)))))))

(module+ test
  (check-equal?
   (value 13) 13
   "Value of a number is just this number")

  (check-equal?
   (value '(+ 1 3)) 4
   "Value of 1 + 3 is 4")

  (check-equal?
   (value '(+ 1 (^ 3 4))) 82
   "Value of 1 + (3 ^ 4) is 82"))



;; sero? :: List -> Bool
(define sero?
  (λ (n)
     (null? n)))

(module+ test
  (check-true
    (sero? '())
    "() stands for zero")

  (check-false
    (sero? '(() ()))
    "(() ()) stands for two"))



;; edd1 :: List -> List
(define edd1
  (λ (n)
     (cons '() n)))

(module+ test
  (check-equal?
    (edd1 '())
    '(())
    "0 + 1 -> 1")

  (check-equal?
    (edd1 '(() ()))
    '(() () ())
    "2 + 1 -> 3"))



;; zub1 :: List -> List
(define zub1
  (λ (n)
     (cdr n)))

(module+ test
  (check-equal?
    (zub1 '(()))
    '()
    "1 - 1 -> 0")

  (check-equal?
    (zub1 '(() () ()))
    '(() ())
    "3 - 1 -> 2"))



;; oo+ :: List -> List
(define oo+
  (λ (n m)
     (cond
      ((sero? m) n)
      (else
        (edd1 (oo+ n (zub1 m)))))))

(module+ test
  (check-equal?
    (oo+ '() '())
    '()
    "0 + 0 -> 0")

  (check-equal?
    (oo+ '(()) '())
    '(())
    "1 + 0 -> 1")

  (check-equal?
    (oo+ '(() () ()) '(() ()))
    '(() () () () ())
    "3 + 2 -> 5"))
