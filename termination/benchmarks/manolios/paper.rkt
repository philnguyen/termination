#lang racket/base

(require "../../main.rkt")

(define ≺/abs
  (λ (x y) (< (abs x) (abs y))))

(let () ; Fig 2
  (define (f x)
    (cond [(or (not (integer? x)) (= x 0)) 0]
          [(< x 0) (f (+ x 1))]
          [else (f (- x 1))]))
  (define (dec n)
    (cond [(or (not (integer? n)) (<= n 0)) 255]
          [else (- n 1)]))
  (define (foo i j)
    (if (= i 1)
        (if (= j 1) 0 (foo (dec j) (dec j)))
        (foo (dec i) j)))
  (with-custom-< ≺/abs
    (begin/termination (f -10))
    (begin/termination (f 10))
    (begin/termination (foo 4 5))
    ;(begin/termination (foo -3 -2)) ; fail :(
    ))

(let () ; fig 6
  (define (g x) (f (+ x 1)))
  (define (h x) (f (- x 1)))
  (define (f x)
    (cond [(= x 0) 0]
          [(< x 0) (g x)]
          [else (h x)]))
  (with-custom-< ≺/abs
    (begin/termination (f 4))
    (begin/termination (f -3))))

(let () ; fig 7
  (define (f x)
    (cond [(<= x 1) 0]
          [(= 1 (modulo x 2)) (f (+ x 1))]
          [else (+ 1 (f (quotient x 2)))]))
  (begin/termination (f 4))
  (begin/termination (f -4)))
