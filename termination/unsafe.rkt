#lang racket/base

(provide
 (except-out (all-from-out racket/base) #%module-begin)
 (rename-out [-module-begin #%module-begin])
 (rename-out [r:terminating-function/c terminating-function/c])
 define/termination
 begin/termination
 (rename-out [with-<? with-custom-<]))

(require (for-syntax racket/base
                     racket/set
                     racket/syntax
                     syntax/parse
                     racket/match
                     racket/contract
                     racket/pretty
                     racket/splicing
                     "syntax-utils.rkt"
                     "flow-analysis/main.rkt"
                     "flow-analysis/parse.rkt")
         (prefix-in r: "runtime-utils.rkt")
         racket/unsafe/ops
         "unsafe-apply-with-termination.rkt"
         )

(begin-for-syntax

  ;; Parameter keeping track of whether there is a pending (non-trivial) application in the same λ-body
  (define app-later? (make-parameter #t))
  (define-syntax-rule (with-acc-app-later? (acc ...) e ...)
    (parameterize ([app-later? (or acc ... (app-later?))])
      e ...))

  (define loop-entries (make-parameter #f))
  (define app-tags (make-parameter #f))

  (define orig-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference)))

  ;; Translate module-level-form
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
       (define-values (_ rhs*) (on-expr #'rhs))
       #;(begin
         (printf "original:~n")
         (pretty-print (syntax->datum #'(define-values lhs rhs)))
         (printf "expanded:~n")
         (pretty-print (syntax->datum #`(define-values lhs #,rhs*)))
         (printf "~n"))
       #`(define-values lhs #,rhs*)]
      [expr
       (define-values (_ expr*) (on-expr #'expr))
       expr*]))

  ;; Translate expression
  (define/contract (on-expr stx)
    (syntax? . -> . (values boolean? syntax?))
    (define (rearm new-stx) (syntax-rearm new-stx stx))
    (define-values (stx-has-app? stx*)
      (syntax-parse (syntax-disarm stx orig-insp)
        #:literals (#%plain-lambda case-lambda if begin begin0 let-values letrec-values
                    set! quote quote-syntax with-continuation-mark #%plain-app
                    #%top #%variable-reference)
        [(~and orig (#%plain-lambda fmls body ...))
         (define-values (_ body*)
           (parameterize ([app-later? #f])
             (on-exprs (syntax->list #'(body ...))))) 
         (values #f (with-syntax-source #'orig #`(#%plain-lambda fmls #,@body*)))]
        [(case-lambda (fmls body ...) ...)
         (with-syntax ([((body* ...) ...)
                        (parameterize ([app-later? #f])
                          (for/list ([clause-body (syntax->list #'((body ...) ...))])
                            (define-values (_ clause-body*)
                              (on-exprs (syntax->list clause-body)))
                            clause-body*))])
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
        [(~and app (#%plain-app fun arg ...))
         (syntax-parse #'fun
           ;; TODO: flow analysis result will eventually subsume this
           ;; Right now it subsumes `:fin` but not macro-generated applications
           [(~or _:fin (~literal terminating-function))
            (match-define-values (app? fun-arg) (on-exprs (syntax->list #'(fun arg ...))))
            (values app? #`(#%plain-app #,@fun-arg))]
           ;; When function can only be wrapped in (term/c _)
           [_
            #:when (equal? (hash-ref (app-tags) (syntax-loc #'app)) {set 'term/c})
            (match-define-values (app? (cons f xs))
                                 (parameterize ([app-later? #t])
                                   (on-exprs (syntax->list #'(fun arg ...)))))
            (with-syntax ([-apply (if (app-later?) #'apply/guard/restore #'apply/guard)])
              (values #t #`(-apply (unsafe-struct-ref #,f 0) #,@xs)))]
           ;; When function can be loop entry
           [_
            #:when (set-member? (loop-entries) (syntax-loc #'app))
            (match-define-values (app? fun-arg)
                                 (parameterize ([app-later? #t])
                                   (on-exprs (syntax->list #'(fun arg ...)))))
            (match-define-values ((cons f xs) bnds) (gen-bindings fun-arg))
            (with-syntax ([-apply (if (app-later?) #'apply/guard/restore #'apply/guard)])
              (values #t
                      (with-let bnds
                        (let ([tags (hash-ref (app-tags) (syntax-loc #'app))])
                          ;; Reduce tests depending on function tag
                          (cond
                            [(equal? tags {set 'term/c})
                             #`(-apply (unsafe-struct-ref #,f 0) #,@xs)]
                            [(not (set-member? tags 'term/c))
                             #`(if (divergence-ok?)
                                   (#,f #,@xs)
                                   (-apply #,f #,@xs))]
                            [else
                             #`(cond [(r:terminating-function? #,f)
                                      (-apply (unsafe-struct-ref #,f 0) #,@xs)]
                                     [(divergence-ok?)
                                      (#,f #,@xs)]
                                     [else (-apply #,f #,@xs)])])))))]
           [_
            (define-values (app? fun-arg) (on-exprs (syntax->list #'(fun arg ...))))
            (values app? #`(#%plain-app #,@fun-arg))])]
        [e (values #|conservative|# #t #'e)]))
    (values stx-has-app? (rearm stx*)))

  ;; Translate expression list
  (define/contract on-exprs
    ((listof syntax?) . -> . (values boolean? (listof syntax?)))
    (match-lambda
      ['() (values #f '())]
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
    ;; Generate bindings for complex expressions
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
    ;; Wrap block of code with let-bindings if there are any
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
    [(~and orig (_ form ...)) 
     (syntax-parse (local-expand #'(#%plain-module-begin form ...) 'module-begin '())
       #:literals (#%plain-module-begin)
       [(~and stx (#%plain-module-begin form ...))
        #;(begin
          (printf "unparsed:~n")
          (pretty-print (syntax->datum #'stx))
          (define ans (parse-module #'stx))
          (printf "parsed module:~n")
          (pretty-print ans))
        (define-values (flows loops) (do-analyze #'stx))
        (with-syntax ([(form* ...)
                       (parameterize ([loop-entries loops]
                                      [app-tags flows])
                         #;(printf "~a loop entries: ~a~n" (set-count loops) loops)
                         #;(printf "flows:~n")
                         #;(for ([(k v) (in-hash flows)])
                           (printf "- ~a ← ~a~n" k v))
                         (map on-module-level-form (syntax->list #'(form ...))))])
          #;(printf "final program:~n")
          #;(pretty-print (syntax->datum #'(#%plain-module-begin form* ...)))
          #'(#%plain-module-begin form* ...))])]))

(define-syntax define/termination
  (syntax-parser
    [(~and stx (_ (~and lhs (f x ...)) e ...))
     (with-syntax* ([f* (with-syntax-source #'lhs #'(λ (x ...) e ...))]
                    [app (with-syntax-source #'stx #'(terminating-function f*))])
       #'(define f app))]))

(define-syntax begin/termination
  (syntax-parser
    [(~and stx (_ e ...))
     (with-syntax ([f (with-syntax-source #'stx #'(λ () e ...))])
       ;; TODO: how to assign source locations for these 2 applications?
       #'((terminating-function f)))]))

;; Define alias just so it can match in `syntax-parse`
(define terminating-function r:terminating-function)
