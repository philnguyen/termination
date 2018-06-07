#lang racket/base

(require racket/match
         "../../main.rkt")

(define f
  (terminating-function/c
   (match-lambda**
    [(n               0              ) n]
    [(0               (? positive? m)) (f m m)]
    [((? positive? n) (? positive? m)) (f (sub1 m) (sub1 n))])))
(f 10 20)
