#lang racket
(module+ test (require rackunit))


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
