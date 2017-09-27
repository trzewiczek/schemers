#lang racket
(require "chapter-01.rkt"
         "chapter-04.rkt")
(module+ test (require rackunit))


;; rember* :: Atom -> List -> List
(define rember*
  (λ (a l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? a (car l)) (rember* a (cdr l)))
          (else (cons (car l) (rember* a (cdr l))))))
       (else
         (cons (rember* a (car l))
               (rember* a (cdr l)))))))

(module+ test
  (check-equal?
    (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
    '((coffee) ((tea)) (and (hick)))
    "All found elements removed from list"))


;; insertR* :: Atom -> Atom -> List -> List
(define insertR*
  (λ (new old l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? old (car l)) (cons (car l)
                                   (cons new (insertR* new old (cdr l)))))
          (else (cons (car l) (insertR* new old (cdr l))))))
       (else
         (cons (insertR* new old (car l))
               (insertR* new old (cdr l)))))))

(module+ test
  (check-equal?
    (insertR* 'roast 'chuck
              '((how much (wood))
                could
                ((a (wood) chuck))
                (((chuck)))
                (if (a) ((wood chuck)))
                could chuck wood))
    '((how much (wood))
      could
      ((a (wood) chuck roast))
      (((chuck roast)))
      (if (a) ((wood chuck roast)))
      could chuck roast wood)))



;; occur* :: Atom -> List -> List
(define occur*
  (λ (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else
        (+ (occur* a (car l))
           (occur* a (cdr l)))))))

(module+ test
  (check-equal?
    (occur* 'banana
            '((banana)
              (split ((((banana ice)))
                      (cream (banana))
                      sherbet))
              (banana)
              (bread)
              (banana brandy)))
    5))



;; subst* :: Atom -> Atom -> List -> List
(define subst*
  (λ (new old l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l))))))
       (else
         (cons (subst* new old (car l))
               (subst* new old (cdr l)))))))

(module+ test
  (check-equal?
    (subst* 'orange 'banana
            '((banana)
              (split ((((banana ice)))
                      (cream (banana))
                      sherbet))
              (banana)
              (bread)
              (banana brandy)))
    '((orange)
      (split ((((orange ice)))
              (cream (orange))
              sherbet))
      (orange)
      (bread)
      (orange brandy))))



;; insertL* :: Atom -> Atom -> List -> List
(define insertL*
  (λ (new old l)
     (cond
       ((null? l) '())
       ((atom? (car l))
        (cond
          ((eq? old (car l)) (cons new
                                   (cons (car l) (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l))))))
       (else
         (cons (insertL* new old (car l))
               (insertL* new old (cdr l)))))))

(module+ test
  (check-equal?
    (insertL* 'pecker 'chuck
              '((how much (wood))
                could
                ((a (wood) chuck))
                (((chuck)))
                (if (a) ((wood chuck)))
                could chuck wood))
    '((how much (wood))
      could
      ((a (wood) pecker chuck))
      (((pecker chuck)))
      (if (a) ((wood pecker chuck)))
      could pecker chuck wood)))



;; member* :: Atom -> List -> Bool
(define member*
  (λ (a l)
     (cond
       ((null? l) #f)
       ((atom? (car l))
        (cond
          ((eq? a (car l)) #t)
          (else (member* a (cdr l)))))
       (else
         (or (member* a (car l))
             (member* a (cdr l)))))))

(module+ test
  (check-true
    (member* 'chips
             '((potato) (chips ((with) fish) (chips))))))



;; leftmost :: List -> Atom
(define leftmost
  (λ (l)
     (cond
       ((atom? (car l)) (car l))
       (else (leftmost (car l))))))

(module+ test
  (check-equal?
    (leftmost '((potato) (chips ((with) fish) (chips))))
    'potato)

  (check-equal?
    (leftmost '(((hot) (tuna (and))) cheese))
    'hot))



;; eqlist :: List -> List -> Bool
(define eqlist?
  (λ (l1 l2)
     (cond
       ((and (null? l1) (null? l2)) #t)
       ((or (null? l1) (null? l2)) #f)
       (else (and (equal? (car l1) (car l2))
                  (equal? (cdr l1) (cdr l2)))))))

(module+ test
  (check-true
    (eqlist? '(strawberry ice cream)
             '(strawberry ice cream)))

  (check-false
    (eqlist? '(strawberry ice cream)
             '(strawberry cream ice)))

  (check-false
    (eqlist? '(banana ((split)))
             '((banana) (split))))

  (check-false
    (eqlist? '(beef ((sausage)) (and (soda)))
             '(beef ((salami)) (and (soda)))))

  (check-true
    (eqlist? '(beef ((sausage)) (and (soda)))
             '(beef ((sausage)) (and (soda)))))

  (check-true
    (eqlist? '() '())))



;; equal? :: Any -> Any -> Bool
(define equal?
  (λ (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else
        (eqlist? s1 s2)))))

(module+ test
  (check-true
    (equal? 0 0))

  (check-true
    (equal? 'potato 'potato))

  (check-true
    (equal? '(potato (soup)) '(potato (soup))))

  (check-false
    (equal? 0 1))

  (check-false
    (equal? 'potato 'tomato))

  (check-false
    (equal? '(potato (soup)) '(tomato (soup)))))



;; rember :: Atom -> List -> List
(define rember
  (λ (s l)
     (cond
       ((null? l) '())
       ((equal? s (car l)) (cdr l))
       (else (cons (car l)
                   (rember s (cdr l)))))))

(module+ test
  (check-equal?
    (rember 2 '(1 2 3))
    '(1 3))

  (check-equal?
    (rember 'tomato '(a tomato soup))
    '(a soup))

  (check-equal?
    (rember '(tomato) '(a (tomato) soup))
    '(a soup))

  (check-equal?
    (rember '() '(a () soup))
    '(a soup)))

