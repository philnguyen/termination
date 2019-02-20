#lang racket

(require termination)

(define/termination (rev ls) (r1 ls '()))
(define (r1 ls a) (if (null? ls) a (r1 (cdr ls) (cons (car ls) a))))

(rev '(1 2 3 4 5))
