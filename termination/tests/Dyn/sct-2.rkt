#lang racket
(require termination)

(define (f i x) (if (null? i) x (g (cdr i) x i)))
(define (g a b c) (f a (cons b c)))

((terminating-function/c f) '(1 2 3 4) 5)
((terminating-function/c g) '(1 2 3 4) 5 6)
