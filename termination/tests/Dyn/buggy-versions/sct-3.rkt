#lang racket
(require termination)

(define a
  (match-lambda**
   [(0 n) (+ 1 n)]
   [(m 0) (a m 1)]
   [(m n) (a (- m 1) (a m (- n 1)))]))

(require rackunit)
(check-exn exn? (λ () (begin/termination (a 3 1))))
