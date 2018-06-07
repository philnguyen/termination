#lang racket
(require "../../main.rkt")

(define a
  (match-lambda**
   [(0 n) (+ 1 n)]
   [(m 0) (a (- m 1) 1)]
   [(m n) (a (- m 1) (a m (- n 1)))]))
(begin/termination (a 3 1))
