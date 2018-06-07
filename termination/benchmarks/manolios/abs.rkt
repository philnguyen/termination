#lang racket/base

(provide ≺/abs)

(define (≺/abs x y) (< (abs x) (abs y)))
