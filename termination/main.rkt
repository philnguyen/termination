#lang racket/base

(provide (rename-out [-app #%app])
         terminating-function/c
         define/termination
         #;begin/termination ; does't work for some reason
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

;; This doesn't work for some reason
(define-syntax-rule (begin/termination e ...)
  (#%app (terminating-function (λ () e ...))))

(define-syntax-rule (with-call-monitored (f x ...) e ...)
  (parameterize ([call-histories (update-Call-Histories (call-histories) f (list x ...))])
    e ...))

(define-syntax -app
  (syntax-parser
    [(_ fun arg ...)
     (with-syntax ([(x ...) (generate-temporaries #'(arg ...))])
       #'(let ([f fun]
               [x arg] ...)
           (cond
             [(terminating-function? f)
              (let ([f* (terminating-function-unwrapped f)])
                (with-call-monitored (f* x ...)
                  (#%app f* x ...)))]
             [(enforcing-termination?)
              (with-call-monitored (f x ...)
                (#%app f x ...))]
             [else (#%app f x ...)])))]))
