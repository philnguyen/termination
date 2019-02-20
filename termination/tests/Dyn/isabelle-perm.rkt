#lang racket/base

(require termination)

(define/termination (p m n r)
    (cond [(< 0 r) (p m (- r 1) n)]
          [(< 0 n) (p r (- n 1) m)]
          [else m]))
  (p 7 8 9)
