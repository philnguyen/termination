#lang racket/base

(provide (all-defined-out))
(define (≺ x y)
  (define (≼ x y) (or (equal? x y) (≺ x y)))
  (cond [(integer? y) (and (integer? x) (< (abs x) (abs y)))]
        [(pair? y) (or (and (list? x) (list? y) (< (length x) (length y)))
                       (member x y))]
        [else (and (not x) y #t)]))
