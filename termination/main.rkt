#lang racket/base

(provide (rename-out [-app #%app])
         terminating-function/c
         define/termination
         begin/termination
         <?
         )

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "size-change-graph.rkt")

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
  (define-syntax-class fin
    #:description "recognized terminating functions"
    ;; TODO fix ugly hack
    (pattern p:id #:when (with-handlers ([exn? (λ _ #f)])
                           (primitive? (eval (format-id #'dummy "~a" #'p)))))))

(define (should-monitor? f) ; `f` if should, or `#f`
  (cond [(terminating-function? f) (terminating-function-unwrapped f)]
        [(enforcing-termination?) f]
        [else #f]))

(define-syntax -app
  (syntax-parser
    [(_ fun:fin arg ...)
     #'(#%app fun arg ...)]
    [(_ fun arg ...)
     (with-syntax ([(x ...) (generate-temporaries #'(arg ...))])
       #'(let ([f fun]
               [x arg] ...)
           (define f* (should-monitor? f))
           (if f*
               (with-call-monitored f* (list x ...)
                 (λ () (#%app f* x ...)))
               (#%app f x ...))))]))
