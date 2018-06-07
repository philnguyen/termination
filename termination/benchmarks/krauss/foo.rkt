#lang racket/base

(require racket/match
         "../../main.rkt")

(define foo 
    (terminating-function/c
     (match-lambda**
      [(#t (? positive? n) m              ) (foo #t (sub1 n) (add1 m))]
      [(#t 0               m              ) (foo #f 0        m       )]
      [(#f n               (? positive? m)) (foo #f (add1 n) (sub1 m))]
      [(#f n               0              ) n])))
  (with-custom-< (λ (x y)
                   (cond [(and (integer? x) (integer? y)) (< -1 x y)]
                         [else (and (not x) y)]))
    (foo #t 4 5))
