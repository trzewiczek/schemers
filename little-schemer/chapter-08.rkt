#lang racket
(module+ test (require rackunit))



;; rember-f :: (a -> b -> Bool) -> a -> List a -> List a
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



;; insertL-f :: (a -> a -> Bool) -> a -> a -> List a -> List a
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



;; insertR-f :: (a -> a -> Bool) -> a -> a -> List a -> List a
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
