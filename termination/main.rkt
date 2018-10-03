#lang racket/base

;; This is the straightforward, lightweihgt, thread-safe, tail-call preserving implementation
;; using continuation marks

(provide (rename-out [-app #%app])
         terminating-function/c
         define/termination
         begin/termination
         (rename-out [with-<? with-custom-<]))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     "syntax-utils.rkt")
         racket/unsafe/ops
         "runtime-utils.rkt"
         "apply-with-termination.rkt"
         )

(define-syntax define/termination
  (syntax-parser
    [(_ (~and lhs (f x ...)) e ...)
     (with-syntax ([gen-lam (with-syntax-source #'lhs #'(λ (x ...) e ...))])
       #'(define f (terminating-function gen-lam)))]))

(define-syntax-rule (begin/termination e ...)
  (-app (terminating-function (λ () e ...))))

(define-syntax -app
  (syntax-parser
    ;; Skip instrumentation for functions trusted to not cause divergence
    [(_ fun:fin arg ...)
     #'(fun arg ...)]
    ;; For most functions, if termination checking is enabled, check
    [(_ fun arg ...)
     (with-syntax ([(x ...) (generate-temporaries #'(arg ...))])
       #'(let ([f fun]
               [x arg] ...)
           (cond [(terminating-function? f)
                  (apply/termination (unsafe-struct-ref f 0) x ...)]
                 [(divergence-ok?)
                  (f x ...)]
                 [else (apply/termination f x ...)])))]))
