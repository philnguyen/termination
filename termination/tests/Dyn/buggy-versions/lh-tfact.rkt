#lang racket/base

(require racket/match
         termination)

(define tfact
  (terminating-function/c
   (match-lambda**
    [(x 0) x]
    [(x n) (tfact (* n x) n)])))

(require rackunit)
(check-exn exn? (λ () (tfact 5 1)))
