#lang racket/base

(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(require termination "../common.rkt")
(run-bm [n 10000] (begin/termination (fact n)))
