#lang racket/base

(require racket/match
         termination)

(define f
  (terminating-function/c
   (match-lambda**
    [(n               0              ) n]
    [(0               (? positive? m)) (f m m)]
    [((? positive? n) (? positive? m)) (f (sub1 m) n)])))

(require rackunit)
(check-exn exn? (λ () (f 10 20)))
