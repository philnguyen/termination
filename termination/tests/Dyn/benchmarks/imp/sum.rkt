#lang s-exp "../../../../unsafe.rkt"

(define (sum n)
  (if (zero? n) 0 (+ n (sum (- n 1)))))

(require "../common.rkt")
(run-bm [N 1000000] (begin/termination (sum N)))
