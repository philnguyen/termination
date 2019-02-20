#lang racket/base

(require racket/match
         termination)

(define gcd
  (terminating-function/c
   (match-lambda**
    [(a 0) a]
    [(a b) (gcd b (modulo a b))])))
(gcd 102 7)
