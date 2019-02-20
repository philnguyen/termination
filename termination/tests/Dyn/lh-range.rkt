#lang racket/base

(require racket/match
         termination)

(define/termination (range lo hi)
  (with-custom-< (λ (x y) (< (- hi x) (- hi y)))
    (cond [(< lo hi) (cons lo (range (add1 lo) hi))]
          [else '()])))
(range 2 6)
