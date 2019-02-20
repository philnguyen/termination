#lang racket/base

(define (sum n)
  (if (zero? n) 0 (+ n (sum (- n 1)))))

(require "../../../../unsafe.rkt")
(for ([N (in-range 1000000 10000001 1000000)])
  (collect-garbage) (collect-garbage) (collect-garbage)
  (printf "sum ~a~n" N)
  (time (begin/termination (sum N))))
