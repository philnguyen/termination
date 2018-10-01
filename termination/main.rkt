#lang racket/base

(provide (rename-out [-app #%app])
         terminating-function/c
         define/termination
         begin/termination
         (rename-out [with-<? with-custom-<]))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/match)
         racket/unsafe/ops
         "apply-with-termination.rkt"
         ;"apply-with-termination-breaking-tail-calls.rkt"
         )

(struct terminating-function (unwrapped) #:transparent
  #:property prop:procedure 0)

(define (terminating-function/c f)
  (if (or (primitive? f) (terminating-function? f)) f (terminating-function f)))

(begin-for-syntax
  (define (with-syntax-source src stx)
    (datum->syntax src
                   (syntax-e stx)
                   (list (syntax-source src)
                         (syntax-line src)
                         (syntax-column src)
                         (syntax-position src)
                         (syntax-span src)))))

(define-syntax define/termination
  (syntax-parser
    [(_ (~and lhs (f x ...)) e ...)
     (with-syntax ([gen-lam (with-syntax-source #'lhs #'(λ (x ...) e ...))])
       #'(define f (terminating-function gen-lam)))]))

(define-syntax-rule (begin/termination e ...)
  (-app (terminating-function (λ () e ...))))

(begin-for-syntax
  (require racket/base)

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
    #:literals (#%plain-lambda case-lambda)
    (pattern p:id #:when (prim? #'p))
    (pattern (#%plain-lambda _ _:fin-expr ...))
    (pattern (case-lambda [_ :fin-expr ...] ...))))

(define-syntax -app
  (syntax-parser
    [(_ fun:fin arg ...)
     #'(fun arg ...)]
    [(_ fun arg ...)
     (with-syntax ([(x ...) (generate-temporaries #'(arg ...))])
       #'(let ([f fun]
               [x arg] ...)
           (cond [(terminating-function? f)
                  (apply/termination (unsafe-struct-ref f 0) x ...)]
                 [(divergence-ok?)
                  (f x ...)]
                 [else (apply/termination f x ...)])))]))
