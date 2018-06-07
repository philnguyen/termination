#lang racket
(require "../../main.rkt")

(define f
  (match-lambda**
   [(x  '()) x]
   [('() y ) (f y (cdr y))]
   [(x    y) (f y (cdr x))]))
((terminating-function/c f) '(1 2 3) '(4 5 6))
