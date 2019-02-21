#lang racket/base
(require "../common.rkt")

(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(run-bm [n 10000] (fact n))
