#lang racket/base

(require racket/match
         termination)

(define merge
  (terminating-function/c
   (match-lambda**
    [((cons x xs) (cons y ys))
     (cond [(< x y) (cons x (merge xs (cons y ys)))]
           [else (cons y (merge (cons x xs) (cons y ys)))])]
    [('() ys) ys]
    [(xs _) xs])))

(require rackunit)
(check-exn exn? (λ () (merge '(2 4 6) '(3 5))))
