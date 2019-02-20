#lang racket
(require termination)

(define (f a b)
  (if (null? b)
      (g a '())
      (f (cons (car b) a) (cdr b))))
(define (g c d)
  (if (null? c)
      d
      (g (cdr c) (cons (car c) d))))
((terminating-function/c f) '(1 2 3) '(4 5 6))
((terminating-function/c f) '(1 2 3) '(4 5 6))
