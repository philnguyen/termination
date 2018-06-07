#lang racket/base
(require "../../main.rkt"
         "abs.rkt")

(define (f x)
  (cond [(<= x 1) 0]
        [(= 1 (modulo x 2)) (f (+ x 1))]
        [else (+ 1 (f (quotient x 2)))]))
(begin/termination (f 4))
(begin/termination (f -4))
