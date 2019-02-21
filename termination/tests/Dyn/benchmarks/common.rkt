#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (run-bm [n n0] e)
  (for ([n (in-range n0 (+ 1 (* 5 n0)) n0)])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (printf "~a: " n)
    (time e)))

(define (≺ x y)
  (define (≼ x y) (or (equal? x y) (≺ x y)))
  (cond [(integer? y) (and (integer? x) (< (abs x) (abs y)))]
        [(pair? y) (or (and (list? x) (list? y) (< (length x) (length y)))
                       (member x y))]
        [else (and (not x) y #t)]))
