#lang s-exp "../../../../unsafe.rkt"

(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(require "../common.rkt")
(run-bm [N 10000] (begin/termination (fact N)))
