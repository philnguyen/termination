#lang racket
(require termination)

(define (p m n r)
  (cond [(> r 0) (p m r n)]
        [(> n 0) (p r (- n 1) m)]
        [else m]))

(require rackunit)
(check-exn exn? (λ () ((terminating-function/c p) 3 4 5)))
