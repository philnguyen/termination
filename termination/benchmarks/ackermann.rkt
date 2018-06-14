#lang racket
(require "../main.rkt")

(define (a m n)
  (cond [(zero? m) (+ 1 n)]
        [(zero? n) (a (- m 1) 1)]
        [else (a (- m 1) (a m (- n 1)))]))

(collect-garbage) (collect-garbage) (collect-garbage)
(time (begin/termination (a 3 10)))
