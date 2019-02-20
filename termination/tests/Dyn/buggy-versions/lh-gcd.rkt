#lang racket/base

(require racket/match
         termination)

(define gcd
  (terminating-function/c
   (match-lambda**
    [(a 0) a]
    [(a b) (gcd b a)])))

(require rackunit)
(check-exn exn? (λ () (gcd 102 7)))
