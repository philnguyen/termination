#lang racket/base

(require racket/match
         "../../main.rkt")

(define/termination (map f xs)
  (match xs
    [(cons x xs) (cons (f x) (map f xs))]
    [_ '()]))
(map add1 '(1 2 4))
