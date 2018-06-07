#lang racket/base

(require racket/match
         "../../main.rkt")

(define merge
  (terminating-function/c
   (match-lambda**
    [((cons x xs) (cons y ys))
     (cond [(< x y) (cons x (merge xs (cons y ys)))]
           [else (cons y (merge (cons x xs) ys))])]
    [('() ys) ys]
    [(xs _) xs])))

(merge '(2 4 6) '(3 5))
