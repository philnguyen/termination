#lang racket
(require termination)

(define ((g r) a) (r (r a)))
(define (f n) (if (zero? n) (λ (x) (+ 1 x)) (g (f n))))

(require rackunit)
(check-exn exn? (λ () (begin/termination (f 5))))

