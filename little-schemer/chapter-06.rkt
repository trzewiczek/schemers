#lang racket
(module+ test (require rackunit))
(require "chapter-01.rkt"
         "chapter-04.rkt")
(provide 1st-sub-exp 2nd-sub-exp operator)


;; numbered? :: AritmeticExpression -> Bool
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

(define numbered?
  (λ (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))



;; 1st-sub-exp :: AritmeticExpression -> AritmeticExpression
(module+ test
  (check-equal?
    (1st-sub-exp '(+ (* 2 5) (* 3 6)))
    '(* 2 5)
    "1st subexpression of + * 2 5 * 3 6 is * 2 5"))

(define 1st-sub-exp
  (λ (aexp)
     (car (cdr aexp))))



;; 2nd-sub-exp :: AritmeticExpression -> AritmeticExpression
(module+ test
  (check-equal?
    (2nd-sub-exp '(+ (* 2 5) (* 3 6)))
    '(* 3 6)
    "2nd subexpression of + * 2 5 * 3 6 is * 3 6"))

(define 2nd-sub-exp
  (λ (aexp)
     (car (cdr (cdr aexp)))))



;; operator :: AritmeticExpression -> Atom
(module+ test
  (check-equal?
    (operator '(+ (* 2 5) (* 3 6)))
    '+
    "Operator of + * 2 5 * 3 6 is +"))

(define operator
  (λ (aexp)
     (car aexp)))



;; value :: AritmeticExpression -> Number
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



;; sero? :: List -> Bool
(module+ test
  (check-true
    (sero? '())
    "() stands for zero")

  (check-false
    (sero? '(() ()))
    "(() ()) stands for two"))

(define sero?
  (λ (n)
     (null? n)))



;; edd1 :: List -> List
(module+ test
  (check-equal?
    (edd1 '())
    '(())
    "0 + 1 -> 1")

  (check-equal?
    (edd1 '(() ()))
    '(() () ())
    "2 + 1 -> 3"))

(define edd1
  (λ (n)
     (cons '() n)))



;; zub1 :: List -> List
(module+ test
  (check-equal?
    (zub1 '(()))
    '()
    "1 - 1 -> 0")

  (check-equal?
    (zub1 '(() () ()))
    '(() ())
    "3 - 1 -> 2"))

(define zub1
  (λ (n)
     (cdr n)))



;; oo+ :: List -> List
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

(define oo+
  (λ (n m)
     (cond
      ((sero? m) n)
      (else
        (edd1 (oo+ n (zub1 m)))))))

