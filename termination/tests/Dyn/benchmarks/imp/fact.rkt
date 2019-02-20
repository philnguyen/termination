#lang racket/base

(define (fact n)
  (if (zero? n) 1 (* n (fact (- n 1)))))

(require "../../../../unsafe.rkt")
(for ([N (in-range 10000 100001 10000)])
  (collect-garbage) (collect-garbage) (collect-garbage)
  (printf "factorial ~a~n" N)
  (time (begin/termination (fact N))))
