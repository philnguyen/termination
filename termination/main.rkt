#lang racket/base

(provide (rename-out [-app #%app])
         terminating-function/c
         define/termination
         begin/termination
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
    ;; TODO not working
    ;; How to determine "primitive-ness" at compile time?
    (pattern p:id #:when (with-handlers ([exn? (λ _ #f)])
                           (primitive? (eval #'p))))
    (pattern (~literal add1))
    (pattern (~literal sub1))
    (pattern (~literal null?))
    (pattern (~literal cons))
    (pattern (~literal car))
    (pattern (~literal cdr))
    (pattern (~literal +))
    (pattern (~literal -))
    (pattern (~literal *))
    (pattern (~literal zero?))))

(define-syntax -app
  (syntax-parser
    [(_ fun:fin arg ...)
     #'(#%app fun arg ...)]
    [(_ fun arg ...)
     (with-syntax ([(x ...) (generate-temporaries #'(arg ...))])
       #'(let ([f fun]
               [x arg] ...)
           (cond
             [(terminating-function? f)
              (let ([f* (terminating-function-unwrapped f)])
                (with-call-monitored f* (list x ...)
                  (λ () (#%app f* x ...))))]
             [(enforcing-termination?)
              (with-call-monitored f (list x ...)
                (λ () (#%app f x ...)))]
             [else (#%app f x ...)])))]))
