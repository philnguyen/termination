#lang racket
(require "../../main.rkt")

(define (p m n r)
  (cond [(> r 0) (p m (- r 1) n)]
        [(> n 0) (p r (- n 1) m)]
        [else m]))
((terminating-function/c p) 3 4 5)
