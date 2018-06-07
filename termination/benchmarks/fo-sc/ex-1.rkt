#lang racket

(require "../../main.rkt")

(define/termination (rev ls) (r1 ls '()))
(define (r1 ls a) (if (null? ls) a (r1 (cdr ls) (cons (car ls) a))))

(collect-garbage) (collect-garbage)
(time (for ([_ (in-range 10000)])
        (rev '(1 2 3 4 5))))
