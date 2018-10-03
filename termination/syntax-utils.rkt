#lang racket/base

(provide with-syntax-source
         fin)

(require racket/syntax
         syntax/parse
         racket/match
         (for-template racket/base))

(define (with-syntax-source src stx)
  (datum->syntax src
                 (syntax-e stx)
                 (list (syntax-source src)
                       (syntax-line src)
                       (syntax-column src)
                       (syntax-position src)
                       (syntax-span src))))

(define (prim? id)
  (match (identifier-binding id)
    [(cons (app module-path-index-resolve (app resolved-module-path-name name)) _)
     (memq name '(#%kernel #%runtime))]
    [_ #f]))

(define-syntax-class fin-expr
  #:description "recognized terminating expressions"
  #:literals (quote set! #%app if let-values letrec-values begin begin0 with-continuation-mark)
  (pattern _:number)
  (pattern _:boolean)
  (pattern (quote _))
  (pattern (set! _ _:fin-expr))
  (pattern (#%app _:fin _:fin-expr ...))
  (pattern (if _:fin-expr _:fin-expr _:fin-expr))
  (pattern (let-values ([_ _:fin-expr] ...)
             _:fin-expr ...))
  (pattern (letrec-values ([_ _:fin-expr] ...)
             _:fin-expr ...))
  (pattern (begin _:fin-expr ...))
  (pattern (begin0 _:fin-expr ...))
  (pattern (with-continuation-mark _:fin-expr ...)))

(define-syntax-class fin
  #:description "recognized terminating functions"
  #:literals (lambda case-lambda)
  (pattern p:id #:when (prim? #'p))
  (pattern (lambda _ _:fin-expr ...))
  (pattern (case-lambda [_ :fin-expr ...] ...)))
