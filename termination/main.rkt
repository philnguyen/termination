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
         )

(struct terminating-function (unwrapped) #:transparent
  #:property prop:procedure 0)

(define (terminating-function/c f)
  (if (or (primitive? f) (terminating-function? f)) f (terminating-function f)))

(define-syntax-rule (define/termination (f x ...) e)
  (define f (terminating-function (λ (x ...) e))))

(define-syntax-rule (begin/termination e ...)
  (-app (terminating-function (λ () e ...))))

(begin-for-syntax
  (require racket/base)

  (define (prim? id)
    (match (identifier-binding id)
      [(cons (app module-path-index-resolve (app resolved-module-path-name '#%kernel)) _)
       #t]
      [_ #f]))
  
  (define-syntax-class fin
    #:description "recognized terminating functions"
    ;; FIXME ugly hack
    (pattern p:id #:when (prim? #'p))))

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
