#lang racket/base

(require racket/match
         racket/list
         "main.rkt")

(define-syntax-rule (gc)
  (begin (collect-garbage) (collect-garbage) (collect-garbage)))

(define a
  (match-lambda**
   [(0 n) (+ 1 n)]
   [(m 0) (a (- m 1) 1)]
   [(m n) (a (- m 1) (a m (- n 1)))]))

(define (fact n)
  (if (zero? n) 1 (* n (fact (sub1 n)))))

(define (rev ls) (r1 ls '()))
(define (r1 ls a) (if (null? ls) a (r1 (cdr ls) (cons (car ls) a))))
(define l (range 10000))


#|
;; no termination check
(begin
  (gc) (time (void (a 3 5)))
  (gc) (time (void (fact 10000)))
  (gc) (time (void (rev l))))
; cpu time: 0 real time: 1 gc time: 0
; cpu time: 94 real time: 100 gc time: 0
; cpu time: 0 real time: 0 gc time: 0
|#

;; With termination check
(begin
  (gc) (time (void ((terminating-function/c a) 3 5)))
  (gc) (time (void ((terminating-function/c fact) 10000)))
  (gc) (time (void ((terminating-function/c rev) l))))

;; naive implementation
; cpu time: 2985 real time: 2984 gc time: 32
; cpu time: 281 real time: 280 gc time: 79
; cpu time: 29078 real time: 29094 gc time: 0
