#lang racket
(require #;"../../main.rkt")

(define (f i x) (if (null? i) x (g (cdr i) x i)))
(define (g a b c) (f a (cons b c)))

(collect-garbage) (collect-garbage)
(time (for ([_ (in-range 1000)])
        (f #;(terminating-function/c f) '(1 2 3 4) 5)
        (g #;(terminating-function/c g) '(1 2 3 4) 5 6)))
