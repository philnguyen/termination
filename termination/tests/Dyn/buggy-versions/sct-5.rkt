#lang racket
(require termination)

(define f
  (match-lambda**
   [(x  '()) x]
   [('() y ) (f y (cdr y))]
   [(x    y) (f y x)]))

(require rackunit)
(check-exn exn? (λ () ((terminating-function/c f) '(1 2 3) '(4 5 6))))
