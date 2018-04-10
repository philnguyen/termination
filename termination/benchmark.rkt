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

#;(begin
  (gc) (time (void (a 3 5)))
  (gc) (time (void (fact 10000)))
  (gc) (time (void (rev l))))

;; With termination check
(begin
  (gc) (time (void ((terminating-function/c a) 3 10)))
  (gc) (time (void ((terminating-function/c fact) 10000)))
  (gc) (time (void ((terminating-function/c rev) l)))
  )


; No termination check
; cpu time: 0 real time: 1 gc time: 0
; cpu time: 94 real time: 100 gc time: 0
; cpu time: 0 real time: 0 gc time: 0

; Naive implementation
; cpu time: 2985 real time: 2984 gc time: 32
; cpu time: 281 real time: 280 gc time: 79
; cpu time: 29078 real time: 29094 gc time: 0

; Exponentially decreased checks
; cpu time: 359 real time: 363 gc time: 0
; cpu time: 265 real time: 263 gc time: 32
; cpu time: 203 real time: 200 gc time: 15

; Detect loop entry first
; cpu time: 125 real time: 131 gc time: 0
; cpu time: 109 real time: 119 gc time: 0
; cpu time: 47 real time: 47 gc time: 0

; Expansion-time optimization of known non-looping functions
; cpu time: 93 real time: 90 gc time: 0
; cpu time: 109 real time: 115 gc time: 15
; cpu time: 32 real time: 32 gc time: 0

; Get rid of naive `refl-trans` loop
; cpu time: 31 real time: 32 gc time: 0
; cpu time: 125 real time: 115 gc time: 0
; cpu time: 31 real time: 31 gc time: 0
