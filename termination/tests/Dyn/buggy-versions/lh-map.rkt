#lang racket/base

(require racket/match
         termination)

(define/termination (map f xs)
  (match xs
    [(cons x xs*) (cons (f x) (map f xs))]
    [_ '()]))

(require rackunit)
(check-exn exn? (λ () (map add1 '(1 2 4))))
