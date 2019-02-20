#lang racket

(require termination)

(define/termination (rev ls) (r1 ls '()))
(define (r1 ls a) (if (null? ls) a (r1 ls (cons (car ls) a))))

(require rackunit)
(check-exn exn? (λ () (rev '(1 2 3 4 5))))

