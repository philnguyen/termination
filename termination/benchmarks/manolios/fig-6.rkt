#lang racket/base
(require "../../main.rkt"
         "abs.rkt")

(define (g x) (f (+ x 1)))
  (define (h x) (f (- x 1)))
  (define (f x)
    (cond [(= x 0) 0]
          [(< x 0) (g x)]
          [else (h x)]))
  (with-custom-< ≺/abs
    (begin/termination (f 4))
    (begin/termination (f -3)))
