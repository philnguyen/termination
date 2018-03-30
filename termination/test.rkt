#lang racket/base

(require racket/match
         racket/function
         racket/list
         rackunit
         "main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Size-change paper examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ex-1: accumulating parameters
(let ()
  (define/termination (rev ls) (r1 ls '()))
  (define (r1 ls a) (if (null? ls) a (r1 (cdr ls) (cons (car ls) a))))
  (rev '(1 2 3 4 5)))

;; Ex-2: indirect recursion
(let ()
  (define (f i x) (if (null? i) x (g (cdr i) x i)))
  (define (g a b c) (f a (cons b c)))
  ((terminating-function/c f) '(1 2 3 4) 5)
  ((terminating-function/c g) '(1 2 3 4) 5 6))

;; Ex-3: lexically ordered parameters
(let ()
  (define a
    (match-lambda**
     [(0 n) (+ 1 n)]
     [(m 0) (a (- m 1) 1)]
     [(m n) (a (- m 1) (a m (- n 1)))]))
  ((terminating-function/c a) 3 1))

;; Ex-4: permuted parameters
(let ()
  (define (p m n r)
    (cond [(> r 0) (p m (- r 1) n)]
          [(> n 0) (p r (- n 1) m)]
          [else m]))
  ((terminating-function/c p) 3 4 5))

;; Ex-5: permuted and possibly discarded parameters
(let ()
  (define f
    (match-lambda**
     [(x  '()) x]
     [('() y ) (f y (cdr y))]
     [(x    y) (f y (cdr x))]))
  ((terminating-function/c f) '(1 2 3) '(4 5 6)))

;; Ex-6: late starting sequence of descending parameter values
(let ()
  (define (f a b)
    (if (null? b)
        (g a '())
        (f (cons (car b) a) (cdr b))))
  (define (g c d)
    (if (null? c)
        d
        (g (cdr c) (cons (car c) d))))
  ((terminating-function/c f) '(1 2 3) '(4 5 6))
  ((terminating-function/c f) '(1 2 3) '(4 5 6)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Other examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let () ; decreasing on `y` can't fool the checker
  (define/termination (f x y)
    (if (zero? x)
        42
        (f (sub1 y) (add1 x)))) 
  (check-exn exn? (λ () (f 3 3))))

(let () ; factorial ok on naturals, diverges and killed on negatives
  (define/termination (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
  (fact 10)
  (check-exn exn? (λ () (fact -1))))

(let ()
  (define/termination (quicksort l)
    (let go ([l l])
      (match l
        [(cons x xs)
         (define-values (<xs ≥xs) (partition (curry < x) xs))
         (append (go <xs) (list x) (go ≥xs))]
        ['() '()])))
  (define/termination (quicksort:incorrect l)
    (let go ([l l])
      (match l
        [(cons x xs)
         (define-values (<xs ≥xs) (partition (curry < x) xs))
         (append (go <xs) (go (cons x ≥xs)))]
        ['() '()])))
  (quicksort '(1 1 1 1 1))
  (check-exn exn? (λ () (quicksort:incorrect '(1 1 1 1 1)))))
