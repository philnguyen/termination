#lang racket/base

(define (sum n)
  (if (zero? n) 0 (+ n (sum (- n 1)))))

(require termination "../common.rkt")
(run-bm [n 1000000] (begin/termination (sum n)))
