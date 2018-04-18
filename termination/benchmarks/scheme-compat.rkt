#lang racket/base

(provide (rename-out [scheme-if if]))

(define-syntax scheme-if
  (syntax-rules ()
    [(_ i t  ) (if i t (void))]
    [(_ i t e) (if i t e     )]))
