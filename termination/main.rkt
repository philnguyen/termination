#lang racket/base

;; This is the straightforward, lightweihgt, thread-safe, tail-call preserving implementation
;; using continuation marks

(provide (rename-out [-app #%app])
         terminating-function/c
         define/termination
         begin/termination
         (rename-out [with-<? with-custom-<])
         with-argument-transformers)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/pretty
                     "syntax-utils.rkt")
         racket/unsafe/ops
         (only-in racket/function arity-includes?)
         "runtime-utils.rkt"
         "apply-with-termination.rkt"
         )

(define-syntax define/termination
  (syntax-parser
    [(_ (~and lhs (f x ...)) e ...)
     (with-syntax ([gen-lam (with-syntax-source #'lhs #'(λ (x ...) e ...))])
       #'(define f (terminating-function gen-lam)))]))

(define-syntax begin/termination
  (syntax-parser
    [(~and stx (_ e ...))
     (with-syntax ([gen-lam (with-syntax-source #'stx #'(λ () e ...))])
       #'(-app (terminating-function gen-lam)))]))

(define (maybe-unwrap f) (if (terminating-function? f) (unsafe-struct-ref f 0) f))

;; Poor man's contract
(define (protect-arg-transformer v)
  (unless (and (procedure? v) (arity-includes? (procedure-arity v) 1))
    (error 'with-argument-transformers "Expect procedure that handles 1 argument, given ~a" v))
  (λ (xs)
    (define xs* (v xs))
    (unless (list? xs*)
      (error 'with-argument-transformers "Argument transformer ~a should produce a list, given ~a" v xs*))
    xs*))

(define-syntax hash-set**
  (syntax-rules ()
    [(_ h) h]
    [(_ h [f v] b ...) (hash-set** (hash-set h f v) b ...)]))

(define-syntax-rule (with-argument-transformers ([f v] ...) e ...)
  (parameterize ([argument-transformers
                  (hash-set** (argument-transformers) [(maybe-unwrap f) (protect-arg-transformer v)] ...)])
    e ...))

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
