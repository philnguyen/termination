#lang racket/base

(provide (all-defined-out))

(require racket/match)

(define-syntax-rule (run-bm [n n0] e)
  (for ([n (in-range n0 (+ 1 (* 5 n0)) n0)])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (printf "~a: " n)
    (time e)))

(define (≺ x y)
  (define (≼ x y) (or (equal? x y) (≺ x y)))
  (cond [(integer? y) (and (integer? x) (< (abs x) (abs y)))]
        [(pair? y) (or (and (list? x) (list? y) (< (length x) (length y)))
                       (member x y)
                       (e≺ x y))]
        [else (and (not x) y)]))

(define (e≺ e₁ e₂) (< (node-count e₁) (node-count e₂)))
(define node-count
  (match-lambda
    [(cons l r) (+ 1 (node-count l) (node-count r))]
    [_ 1]))
