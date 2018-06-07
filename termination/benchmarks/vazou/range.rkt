#lang racket/base

(require racket/match
         "../../main.rkt")

(define/termination (range lo hi)
  (with-custom-< (λ (x y)
                   (and (integer? x) (integer? y)
                        (< (- hi x) (- hi y))))
    (cond [(< lo hi) (cons lo (range (add1 lo) hi))]
          [else '()])))
(range 2 6)
