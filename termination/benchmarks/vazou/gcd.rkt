#lang racket/base

(require racket/match
         "../../main.rkt")

(define gcd
  (terminating-function/c
   (match-lambda**
    [(a 0) a]
    [(a b) (gcd b (modulo a b))])))
(gcd 102 7)
