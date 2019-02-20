#lang racket/base
(require termination)

(define (f x)
  (cond [(<= x 0) (f (- x 1))]
        [(= 1 (modulo x 2)) (f (+ x 1))]
        [else (+ 1 (f (quotient x 2)))]))

(require rackunit)
(check-exn exn? (λ () (begin/termination (f 4))))
(check-exn exn? (λ () (begin/termination (f -4))))
