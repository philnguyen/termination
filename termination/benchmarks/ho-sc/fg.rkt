#lang racket
(require "../../main.rkt")

(define ((g r) a) (r (r a)))
(define (f n) (if (zero? n) (λ (x) (+ 1 x)) (g (f (- n 1)))))
(begin/termination (f 5))
