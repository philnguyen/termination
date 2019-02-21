#lang racket/base
(require "../common.rkt")

(define (sum n)
  (if (zero? n) 0 (+ n (sum (- n 1)))))

(run-bm [N 1000000] (sum N))
