#lang racket/base

(require termination)

(define/termination (p m n r)
  (cond [(< 0 r) (p m r n)]
        [(< 0 n) (p r (- n 1) m)]
        [else m]))

(require rackunit)
(check-exn exn? (λ () (p 7 8 9)))
