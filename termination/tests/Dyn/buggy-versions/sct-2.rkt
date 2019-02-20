#lang racket
(require termination)

(define (f i x) (if (null? i) x (g i x i)))
(define (g a b c) (f a (cons b c)))

(require rackunit)
(check-exn exn? (λ () ((terminating-function/c f) '(1 2 3 4) 5)))
(check-exn exn? (λ () ((terminating-function/c g) '(1 2 3 4) 5 6)))
