#lang racket/base

(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [-module-begin #%module-begin])
 define/termination
 begin/termination
 (rename-out [with-<? with-custom-<]))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/match
                     racket/contract
                     racket/pretty
                     racket/splicing
                     "syntax-utils.rkt")
         (prefix-in r: "runtime-utils.rkt")
         racket/unsafe/ops
         "unsafe-apply-with-termination.rkt"
         )

(begin-for-syntax

  (define app-later? (make-parameter #f))
  (define-syntax-rule (with-acc-app-later? (acc ...) e ...)
    (parameterize ([app-later? (or acc ... (app-later?))])
      e ...))

  (define on-module-level-form
    (syntax-parser
      #:literals (#%provide begin-for-syntax #%declare module module*
                            define-values define-syntaxes #%require)
      [(~and stx (~or (#%provide _ ...)
                      (begin-for-syntax _ ...)
                      (#%declare _ ...)
                      (module _ ...)
                      (module* _ ...)
                      (define-syntaxes _ ...)
                      (#%require _ ...)))
       #'stx]
      [(define-values lhs rhs)
       (define-values (_ rhs*) (with-acc-app-later? (#t)
                                 (on-expr #'rhs)))
       #`(define-values lhs #,rhs*)]
      [expr
       (define-values (_ expr*) (with-acc-app-later? (#t)
                                  (on-expr #'expr)))
       expr*]))

  (define/contract on-expr
    (syntax? . -> . (values boolean? syntax?))
    (syntax-parser
      #:literals (#%plain-lambda case-lambda if begin begin0 let-values letrec-values
                  set! quote quote-syntax with-continuation-mark #%plain-app #%top #%variable-reference)
      [(#%plain-lambda fmls body ...)
       (define-values (_ body*)
         (parameterize ([app-later? #f])
           (on-exprs (syntax->list #'(body ...)))))
       (values #f #`(#%plain-lambda fmls #,@body*))]
      [(case-lambda (fmls body ...) ...)
       (with-syntax ([((body* ...) ...)
                      (for/list ([clause-body (syntax->list #'((body ...) ...))])
                        (define-values (_ clause-body*) (on-exprs (syntax->list clause-body)))
                        clause-body*)])
         (values #f #'(case-lambda (fmls body* ...) ...)))]
      [(if cnd thn els)
       (define-values (thn-has-app? thn*) (on-expr #'thn))
       (define-values (els-has-app? els*) (on-expr #'els))
       (define-values (cnd-has-app? cnd*)
         (with-acc-app-later? (thn-has-app? els-has-app?)
           (on-expr #'cnd)))
       (values (or cnd-has-app? thn-has-app? els-has-app?)
               #`(if #,cnd* #,thn* #,els*))]
      [((~and seq (~or begin begin0)) body ...)
       (define-values (app? body*) (on-exprs (syntax->list #'(body ...))))
       (values app? #`(seq #,@body*))]
      [((~and let (~or let-values letrec-values))
        ([lhs rhs] ...) body ...)
       (define-values (body-has-app? body*) (on-exprs (syntax->list #'(body ...))))
       (define-values (rhs-has-app? rhs*)
         (with-acc-app-later? (body-has-app?)
           (on-exprs (syntax->list #'(rhs ...)))))
       (with-syntax ([(rhs* ...) rhs*]
                     [(body* ...) body*])
         (values (or rhs-has-app? body-has-app?)
                 #'(let ([lhs rhs*] ...) body* ...)))]
      [(set! x e)
       (define-values (app? e*) (on-expr #'e))
       (values app? #`(set! x #,e*))]
      [(with-continuation-mark k v b)
       (match-define-values (app? (list k* v* b*))
                            (on-exprs (list #'k #'v #'b)))
       (values app? #`(with-continuation-mark #,k* #,v* #,b*))]
      [(~and e (~or _:id
                    (quote _)
                    (quote-syntax _)
                    (#%top _)
                    (#%variable-reference _ ...)))
       (values #f #'e)]
      [(#%plain-app fun arg ...)
       (match-define-values (app? (cons fun* arg*))
                            (on-exprs (syntax->list #'(fun arg ...))))
       (syntax-parse fun*
         [(~or _:fin (~literal terminating-function)) (values app? #`(#%plain-app #,fun* #,@arg*))]
         [_
          (match-define-values ((cons f xs) bnds) (gen-bindings (cons fun* arg*)))
          (with-syntax ([-apply (if (or app? (app-later?)) #'apply/guard/restore #'apply/guard)])
            (values #t
                    (with-let bnds
                      #`(cond [(r:terminating-function? #,f)
                               (-apply (unsafe-struct-ref #,f 0) #,@xs)]
                              [(divergence-ok?)
                               (#,f #,@xs)]
                              [else (-apply #,f #,@xs)]))))])]
      [e (values #|conservative|# #t #'e)]))

  (define/contract on-exprs
    ((listof syntax?) . -> . (values boolean? (listof syntax?)))
    (match-lambda
      ['() (values (app-later?) '())]
      [(list e) (define-values (app? e*) (on-expr e))
                (values app? (list e*))]
      [(cons e es)
       (define-values (rest-has-app? es*) (on-exprs es))
       (define-values (first-has-app? e*)
         (with-acc-app-later? (rest-has-app?)
           (on-expr e)))
       (values (or first-has-app? rest-has-app?)
               (cons e* es*))]))

  (define/contract gen-bindings
    ((listof syntax?) . -> . (values (listof syntax?) (listof (cons/c identifier? syntax?))))
    (match-lambda
      ['() (values '() '())]
      [(cons e es)
       (define-values (es* bnds) (gen-bindings es))
       (syntax-parse e
         [(~and v (~or _:id (quote _)))
          (values (cons #'v es*) bnds)]
         [_
          (define x (generate-temporary))
          (values (cons x es*) (cons (cons x e) bnds))])]))

  (define/contract with-let
    ((listof (cons/c identifier? syntax?)) syntax? . -> . syntax?)
    (match-lambda**
     [('() e) e]
     [(bnds e)
      (define-values (lhs rhs)
        (for/lists (lhs rhs) ([bnd (in-list bnds)])
          (values (car bnd) (cdr bnd))))
      (with-syntax ([(lhs ...) lhs]
                    [(rhs ...) rhs])
        #`(let ([lhs rhs] ...) #,e))]))
  )

(define-syntax -module-begin
  (syntax-parser
    [(_ form ...)
     (syntax-parse (local-expand #'(#%plain-module-begin form ...) 'module-begin '())
       #:literals (#%plain-module-begin)
       [(#%plain-module-begin form ...)
        (with-syntax ([(form* ...) (map on-module-level-form (syntax->list #'(form ...)))])
          #'(#%plain-module-begin form* ...))])]))

(define-syntax define/termination
  (syntax-parser
    [(_ (~and lhs (f x ...)) e ...)
     (with-syntax ([f* (with-syntax-source #'lhs #'(λ (x ...) e ...))])
       #'(define f (terminating-function f*)))]))

(define-syntax-rule (begin/termination e ...)
  ((terminating-function (lambda () e ...))))

;; Define alias just so it can match in `syntax-parse`
(define terminating-function r:terminating-function)
