#lang racket
(require termination)

(define (f a b)
  (if (null? b)
      (g a '())
      (f (cons (car b) a) b)))
(define (g c d)
  (if (null? c)
      d
      (g c (cons (car c) d))))

(require rackunit)
(check-exn exn? (λ () ((terminating-function/c f) '(1 2 3) '(4 5 6))))
(check-exn exn? (λ () ((terminating-function/c f) '(1 2 3) '(4 5 6))))

