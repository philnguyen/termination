#lang racket
(require termination)

(define (Y f) ((λ (q) (f (λ (s) ((q q) s)))) (λ (q) (f (λ (s) ((q q) s))))))
(define (((h b) f) n) (if (zero? n) (f 1) (f ((b f) (sub1 n)))))
(define ((g a) m) (if (zero? m) add1 ((Y h) (a m))))
(define (ack m n) (((Y g) m) n))

(require rackunit)
(check-exn exn? (λ () (begin/termination (ack 3 1))))
