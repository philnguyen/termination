#lang racket/base

(require racket/match
         "../../main.rkt")

(define tfact
  (terminating-function/c
   (match-lambda**
    [(x 0) x]
    [(x n) (tfact (* n x) (- n 1))])))
(tfact 5 1)