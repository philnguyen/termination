#lang typed/racket/base

(provide define-parameter)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         syntax/parse/define)

(define-syntax define-parameter
  (syntax-parser
    [(_ x:id (~literal :) T e)
     (with-syntax ([with-x (format-id #'x "with-~a" #'x)])
       #'(begin
           (define key ((inst make-continuation-mark-key T) 'x))
           (define x
             (let ([default : T e])
               (λ () (continuation-mark-set-first #f key default))))
           (define-syntax-rule (with-x new-x body (... ...))
             (with-continuation-mark key new-x (let () body (... ...))))))]))
