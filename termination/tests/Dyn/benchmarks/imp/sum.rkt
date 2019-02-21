#lang racket/base

(define (sum n)
  (if (zero? n) 0 (+ n (sum (- n 1)))))

(require "../../../../unsafe.rkt" "../common.rkt")
(run-bm [N 1000000] (begin/termination (sum N)))
