#lang racket/base

(provide parse-module
         syntax-loc)

(require racket/match
         racket/set
         racket/syntax
         racket/contract
         syntax/parse
         syntax/id-table
         syntax/id-set
         "lang.rkt"
         (for-template racket/base))

(define (parse-module stx)
  (parameterize ([id-occurence-count (make-hasheq)])
    (syntax-parse stx
      [(_ form ...)
       (define tops
         (for/fold ([acc (immutable-free-id-set)])
                   ([form (in-list (syntax->list #'(form ...)))])
           (syntax-parse form
             [(define-values (x:id ...) _)
              (for/fold ([acc acc])
                        ([x (in-list (syntax->list #'(x ...)))])
                (free-id-set-add acc x))]
             [_ acc])))
       (define provides
         (for/fold ([acc (seteq)])
                   ([form (in-list (syntax->list #'(form ...)))])
           (syntax-parse form
             [((~literal #%provide) x:id ...)
              (set-union acc (list->seteq (map syntax-e (syntax->list #'(x ...)))))]
             [_ acc])))
       (define clauses
         (parameterize ([top-level-ids tops])
           (map parse-module-level-form (syntax->list #'(form ...)))))
       (M clauses provides)])))

(define parse-module-level-form
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
     (Base (void))] 
    [(define-values (x:id ...) rhs)
     (Define-Values
       (map syntax-e (syntax->list #'(x ...)))
       (parse-expr #'rhs))]
    [expr (parse-expr #'expr)]))

(define parse-expr
  (syntax-parser
    #:literals (#%plain-lambda case-lambda if begin begin0 let-values letrec-values
                               set! quote quote-syntax with-continuation-mark #%plain-app
                               #%top #%variable-reference #%expression)
    [(#%expression e) (parse-expr #'e)]
    [(~and lam (#%plain-lambda fmls body ...+))
     (define-values (xs ρ) (parse-formals #'fmls))
     (Lam xs (with-env ρ (parse-expr #'(begin body ...))) (syntax-loc #'lam))]
    [(case-lambda (fmls body ...) ...)
     (Case-Lam
      (for/list ([clause (in-list (syntax->list #'((fmls body ...) ...)))])
        (syntax-parse clause
          [(fmls body ...)
           (parse-expr (with-syntax-source clause
                         #'(#%plain-lambda fmls body ...)))])))]
    [(if cnd thn els)
     (If (parse-expr #'cnd) (parse-expr #'thn) (parse-expr #'els))]
    [(begin body ...)
     (Begin (map parse-expr (syntax->list #'(body ...))))]
    [(begin0 body0 body ...)
     (Begin0 (parse-expr #'body0)
             (Begin (map parse-expr (syntax->list #'(body ...)))))]
    [(let-values (bindings ...) body ...)
     (define-values (bindings-rev ρ)
       (for/fold ([bindings-rev '()] [ρ (env)])
                 ([bnd (in-list (syntax->list #'(bindings ...)))])
         (syntax-parse bnd
           [((x ...) e)
            (define-values (xs ρ*) (parse-formals #'(x ...) #:base ρ))
            (values (cons (Bnd (Fmls-init xs) (parse-expr #'e)) bindings-rev) ρ*)])))
     (Let (reverse bindings-rev)
          (with-env ρ (Begin (map parse-expr (syntax->list #'(body ...))))))]
    [(letrec-values (bindings ...) body ...)
     (define-values (lhss-rev ρ)
       (for/fold ([lhss-rev '()] [ρ (env)])
                 ([bnd (in-list (syntax->list #'(bindings ...)))])
         (syntax-parse bnd
           [((x ...) _)
            (define-values (lhs ρ*) (parse-formals #'(x ...) #:base ρ))
            (values (cons (Fmls-init lhs) lhss-rev) ρ*)])))
     (Letrec
      (for/list ([lhs (in-list (reverse lhss-rev))]
                 [bnd (in-list (syntax->list #'(bindings ...)))])
        (syntax-parse bnd
          [(_ eₓ) (Bnd lhs (with-env ρ (parse-expr #'eₓ)))]))
      (with-env ρ (Begin (map parse-expr (syntax->list #'(body ...))))))]
    [(set! x e)
     (define x-name (syntax-e #'x))
     (Set! (cond [(free-id-table-ref (env) #'x #f) => Lex-Ref]
                 [else (Top-Ref x-name)])
           (parse-expr #'e))]
    [(with-continuation-mark k v b)
     (Wcm (parse-expr #'k) (parse-expr #'v) (parse-expr #'b))]
    [(quote x)
     (Base (syntax-e #'x))]
    [(~and stx (#%plain-app fun arg ...))
     (App (parse-expr #'fun)
          (map parse-expr (syntax->list #'(arg ...)))
          (syntax-loc #'stx))]
    [x:id
     (define x-name (syntax-e #'x))
     (cond
       [(free-id-table-ref (env) #'x #f) => Lex-Ref]
       [(free-id-set-member? (top-level-ids) #'x) (Top-Ref x-name)]
       [else (Prim x-name)])] 
    [e
     (log-warning "don't know how to parse ~a~n" (syntax->datum #'e))
     (Opq)]))

(define top-level-ids (make-parameter #f))
(define env (make-parameter (make-immutable-free-id-table)))
(define-syntax-rule (with-env ρ e ...) (parameterize ([env ρ]) e ...))
(define/contract (parse-formals fmls #:base [ρ₀ (env)])
  (->* [syntax?] [#:base any/c] (values Fmls? any/c))
  (define (parse-binder id ρ)
    (define x (inc-id! id))
    (values x (free-id-table-set ρ id x)))
  (define (parse-binders ids ρ)
    (let loop ([ids ids] [ρ ρ] [xs-rev '()])
      (match ids
        ['() (values (reverse xs-rev) ρ)]
        [(cons id ids*)
         (define-values (x ρ*) (parse-binder id ρ))
         (loop ids* ρ* (cons x xs-rev))])))
  (syntax-parse fmls
    [(x:id ...)
     (define-values (xs ρ) (parse-binders (syntax->list #'(x ...)) ρ₀))
     (values (Fmls xs #f) ρ)]
    [rest:id
     (define-values (r ρ) (parse-binder #'rest ρ₀))
     (values (Fmls '() r) ρ)]
    [(x:id ... . rest:id)
     (define-values (inits ρ₁) (parse-binders (syntax->list #'(x ...)) ρ₀))
     (define-values (rest-id ρ₂) (parse-binder #'rest ρ₁))
     (values (Fmls inits rest-id) ρ₂)]))

(define id-occurence-count (make-parameter #f))
(define (inc-id! id)
  (define m (id-occurence-count))
  (define s (syntax-e id))
  (define old-count (hash-ref m s 0))
  (define name
    (case old-count
      [(0) s]
      [else (format-symbol "~a~a" s (n-sub old-count))]))
  (hash-set! m s (+ 1 old-count))
  name)

(define n-sub
  (let ([pool #(₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉)])
    (define (go n)
      (if (<= n 9)
          (vector-ref pool n)
          (let-values ([(q r) (quotient/remainder n 10)])
            (format-symbol "~a~a" (go q) (go r)))))
    (λ (n)
      (if (>= n 0) (go n) (format "⁻~a" (go (- n)))))))

(define (syntax-loc stx) (L (syntax-line stx) (syntax-column stx)))

(define (with-syntax-source src stx)
  (datum->syntax src
                 (syntax-e stx)
                 (list (syntax-source src)
                       (syntax-line src)
                       (syntax-column src)
                       (syntax-position src)
                       (syntax-span src))))
