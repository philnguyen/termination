#lang typed/racket/base

(provide analyze)

(require/typed racket/function
  [arity-includes? ((U Natural arity-at-least) Natural → Boolean)])

(require racket/match
         racket/set
         racket/list
         racket/pretty
         set-extras
         unreachable
         "lang.rkt"
         "utils.rkt")

;; Dynamic parameter keeping track of function body current state comes from.
;; This is an abstraction of the current expression, so there's no need
;; to save as part of the state
(define enclosing ((inst make-parameter L) (L -2 -2)))

(: analyze : M → (Values S (℘ L)))
;; Compute approximate:
;; - Function tags that flow to location `L` for each application `(Eₕ Eₓ…)ᴸ`
;; - Set of locations `L` of applications `(Eₕ Eₓ…)ᴸ` that can start loops
(define (analyze M)
  (define ith : Integer 1)
  (let loop ([$ : $ (hash)] [Σ : Σ (hash)] [G : G (hash)])
    (begin (printf "iter ~a~n" ith)
           (set! ith (+ 1 ith)))
    (define-values ($* Σ* G*) (ev-M $ (hash) Σ G M))
    (if (and (equal? $ $*) (equal? Σ Σ*) (equal? G G*))
        (summarize $ Σ G)
        (loop $* Σ* G*))))

(: ev-M : $ $ Σ G M → (Values $ Σ G))
;; Evaluate module
(define (ev-M $ᵢₙ $ₒᵤₜ Σ G M₀)

  ;; One-off implementation:
  ;; - Hard-coded 0-CFA (no environment, no alloc context, lambda is closure)
  ;; - Where normal people use reader monad, it uses dynamic parameters
  ;; - Where normal people use state monad, it uses `set!` (`$ₒᵤₜ`, `Σ`, `G`)

  (: ev-D! : D → Boolean)
  ;; Evaluate module-level form, returns `#t` if plausibly succeeds
  (define ev-D!
    (match-lambda
      [(Define-Values Xs E)
       (define rhs (ev-E! E))
       (define As (map Top-Ref Xs))
       (match (?alloc As rhs Σ)
         [(? values Σ*) (set! Σ Σ*) #t]
         [#f            #f])]
      [(? E? E)
       (not (set-empty? (ev-E! E)))]))

  (: ev-E! : E → R)
  ;; Evaluate expression
  (define (ev-E! E₀)
    (cond
      [(hash-ref $ₒᵤₜ E₀ #f) => values]
      [else
       (set! $ₒᵤₜ (hash-set $ₒᵤₜ E₀ (cache-ref $ᵢₙ E₀))) 
       (define Rₐ
         (match E₀
           [(or (? Lam?) (? Case-Lam?) (? Prim?) (? Base?))
            {set (list {set E₀})}]
           [(or (? Top-Ref?) (? Lex-Ref?))
            {set (list (hash-ref Σ E₀))}]
           [(If Eᵢ Eₜ Eₑ)
            (define-values (tt? ff?) (check-plausible (ev-E! Eᵢ)))
            (∪ (if tt? (ev-E! Eₜ) ∅)
               (if ff? (ev-E! Eₑ) ∅))]
           [(Begin Es)
            (match (ev-E!* Es)
              [#f ∅]
              ['() -Void]
              [(? pair? Ws) (last Ws)])]
           [(Begin0 E₀ E)
            (define W₀ (ev-E! E₀))
            (cond [(set-empty? W₀)       ∅]
                  [(set-empty? (ev-E! E)) ∅]
                  [else                  W₀])]
           ;; With mono-variant, `let` and `letrec` are similar
           [(or (Let Bnds E) (Letrec Bnds E))
            #:when (and Bnds E)
            (if (andmap ev-Bnd! Bnds)
                (ev-E! E)
                ∅)]
           [(Set! X E*)
            (match (filter-arity (ev-E! E*) 1)
              [(? set-empty?) ∅]
              [W (for ([V-list (in-set W)])
                   (set! Σ (⊔ X (car V-list) Σ)))
                 -Void])]
           [(Wcm Eₖ Eᵥ E)
            (ev-E! Eₖ)
            (ev-E! Eᵥ)
            ;; relies on continuation-mark-get being opaque
            (ev-E! E)]
           [(App Eₕ Eₓs L)
            (match (ev-E!* (cons Eₕ Eₓs))
              [(? values (app (λ (Ws) (guard-arity Ws 1)) (cons Vₕ^ Wₓ)))
               (for/R ([Vₕ (in-set Vₕ^)]) (app! Vₕ Wₓ L))]
              [#f ∅])]))
       (set! $ₒᵤₜ ($⊔ E₀ Rₐ $ₒᵤₜ))
       Rₐ]))

  (: ev-Bnd! : Bnd → Boolean)
  ;; Evaluate binding, returns `#t` if plausibly succeeds
  (define (ev-Bnd! bnd)
    (match-define (Bnd Xs E) bnd)
    (match (?alloc (map Lex-Ref Xs) (ev-E! E) Σ)
      [(? values Σ*) (set! Σ Σ*) #t]
      [#f #f]))

  (: app! : V W L → R)
  (define (app! Vₕ Wₓ Lₑᵣ)
    (match Vₕ
      [(Prim o) (δ o Wₓ Lₑᵣ)]
      [(? Lam?) (app-Lam! Vₕ Wₓ Lₑᵣ)]
      [(Case-Lam Lams)
       (match ((inst findf Lam)
               (let ([n (length Wₓ)])
                 (λ (l) (arity-includes? (Fmls-arity (Lam-_0 l)) n)))
               Lams)
         [(? values Vₕ*) (app-Lam! Vₕ* Wₓ Lₑᵣ)]
         [#f ∅])]
      [(Str 'terminating-function (list A))
       (for/R ([Vₕ* (in-set (hash-ref Σ A mk-∅))])
         (app! Vₕ* Wₓ Lₑᵣ))]
      [(Opq) (havoc! Wₓ)]
      [_ #;(printf "ignoring application of ~a~n" Vₕ) ∅]))

  (: app-Lam! : Lam W L → R)
  (define (app-Lam! Vₕ Wₓ Lₑᵣ) 
    (match-define (Lam Xs E Lₑₑ) Vₕ)
    (if (arity-includes? (Fmls-arity Xs) (length Wₓ))
        (match-let ([(Fmls Xs:init X:rest) Xs])
          (set! G (G⊔¹ (enclosing) Lₑₑ G))
          (alloc! Xs Wₓ)
          (parameterize ([enclosing Lₑₑ])
            (ev-E! E)))
        ∅))

  (: ev-E!* : (Listof E) → (Option (Listof (℘ W))))
  (define (ev-E!* Es)
    (let loop ([Es : (Listof E) Es] [rev-Ws : (Listof (℘ W)) '()])
      (match Es
        ['() (reverse rev-Ws)]
        [(cons E₁ Es*)
         (match (ev-E! E₁)
           [(? set-empty?) #f]
           [W₁ (loop Es* (cons W₁ rev-Ws))])])))

  (: δ : Symbol W L → R)
  (define (δ o W L)

    (define (mk-struct [tag : StrTag])
      (define As ((inst build-list A) (length W) (λ (i) (A:Field tag i L))))
      (set! Σ (foldl ⊔ Σ As W))
      (list {set (Str tag As)}))

    (define (st-ac [tag : StrTag] [ith : Integer] [Vs : (℘ V)])
      (for/V^ ([V (in-set Vs)])
        (match V
          [(Str (== tag) As) (hash-ref Σ (list-ref As ith) mk-∅)]
          [(Opq) {set (Fo-Opq)}]
          [_ ∅])))
    
    (match* (o W)
      [('values W) {set W}]
      [('terminating-function W) {set (mk-struct 'terminating-function)}]
      [((or 'cons 'pair) W) {set (mk-struct 'cons)}]
      [('car (list V^)) {set (list (st-ac 'cons 0 V^))}]
      [('cdr (list V^)) {set (list (st-ac 'cons 1 V^))}]
      [('make-vector Vs)
       (define A (A:Vectof-Field L))
       (define Vₑ (match Vs
                    [(list Vₙ) {set (Base 0)}]
                    [(list Vₙ Vₓ) Vₓ]
                    [_ ∅]))
       (cond [(set-empty? Vₑ) ∅]
             [else (set! Σ (⊔ A Vₑ Σ))
                   {set (list {set (Vectof A)})}])]
      [('vector Vs)
       (define As ((inst build-list A) (length Vs) (λ (i) (A:Vect-Field i L))))
       (set! Σ (foldl ⊔ Σ As Vs))
       {set (list {set (Vect As)})}]
      [('vector-ref (list Vᵥ Vᵢ))
       (define Vₐ
         (for/V^ ([Vᵥ (in-set Vᵥ)])
                 (match Vᵥ
                   [(Vectof A) (hash-ref Σ A mk-∅)]
                   [(Vect As)
                    (define n (length As))
                    (for/V^ ([Vᵢ (in-set Vᵢ)])
                            (match Vᵢ
                              [(Base (? index? i)) (if (< i n) (hash-ref Σ (list-ref As i) mk-∅) ∅)]
                              [_ (for/V^ ([Aᵢ (in-list As)])
                                         (hash-ref Σ Aᵢ mk-∅))]))]
                   [(or (Opq) (Fo-Opq)) {set (Fo-Opq)}]
                   [_ ∅])))
       {set (list Vₐ)}]
      [('vector-set! (list Vᵥ Vᵢ Vₓ))
       (for ([Vᵥ (in-set Vᵥ)])
         (match Vᵥ
           [(Vectof A) (set! Σ (⊔ A Vₓ Σ))]
           [(Vect As)
            (define n (length As))
            (for ([Vᵢ (in-set Vᵢ)])
              (match Vᵢ
                [(Base (? index? i))
                 (when (< i n)
                   (set! Σ (⊔ (list-ref As i) Vₓ Σ)))]
                [_ (for ([Aᵢ (in-list As)])
                     (set! Σ (⊔ Aᵢ Vₓ Σ)))]))]
           [_ (void)]))
       -Void]
      [('apply (cons Vₕ^ Wₓ))
       (for/R ([Vₕ (in-set Vₕ^)])
         (app! Vₕ Wₓ L))]
      [((? non-havocking?) _) {set (list {set (Fo-Opq)})}]
      [(o W) (havoc! W)]))

  (: alloc! : Fmls W → Void)
  (define (alloc! Xs W) 
    (match-define (Fmls Xs:init X:rest) Xs)
    (define-values (W:init W:rest) (split-at W (length Xs:init)))
    (set! Σ (foldl ⊔ Σ (map Lex-Ref Xs:init) W:init))
    (when X:rest
      (alloc-var! X:rest W:rest)))

  (: alloc-var! : Symbol W → Void)
  (define (alloc-var! X W)
    (define A (Lex-Ref X))
    (define A:elem (A:Var X))
    (set! Σ (⊔¹ A (Base '()) Σ))
    (when (pair? W)
      (set! Σ (⊔¹ A (Str 'cons (list A:elem A)) Σ))
      (for ([V (in-list W)])
        (set! Σ (⊔ A:elem V Σ)))))

  (: havoc! : W → R)
  (define (havoc! W)
    (set! Σ (foldl leak Σ W))
    (set! G (G⊔¹ (enclosing) (L -1 -1) G))
    (parameterize ([enclosing (L -1 -1)])
      (set-for-each (hash-ref Σ (A:Havoc) mk-∅) havoc-V!))
    {set (list {set (Opq)})})

  (: havoc-V! : V → R)
  (define havoc-V!
    (match-lambda
      [(and lam (Lam Fmls _ _))
       (app-Lam! lam (make-havoc-args Fmls) (L -1 -1))]
      [V
       {set (list {set (Opq)})}]))

  (match-let ([(M Ds Xs) M₀])
    ;; Execute each non-provide statement,
    ;; then havoc each binding if these statements plausibly succeed
    (when (andmap ev-D! Ds)
      (define exports
        (for/V^ ([X (in-set Xs)] #:when (hash-has-key? Σ (Top-Ref X)))
          (hash-ref Σ (Top-Ref X))))
      (havoc! (list exports)))
    (values $ₒᵤₜ Σ G)))

(: leak : V^ Σ → Σ)
(define (leak V^ Σ) (⊔ (A:Havoc) V^ Σ))

(: check-plausible : (℘ W) → (Values Boolean Boolean))
(define (check-plausible W)
  (for*/fold ([tt? : Boolean #f] [ff? : Boolean #f])
             ([Vs (in-set W)]
              #:when (= 1 (length Vs))
              [V (in-set (car Vs))])
    (match V
      [(Base #f) (values tt? #t)]
      [(or (Fo-Opq) (Opq)) (values #t #t)]
      [_ (values #t ff?)])))

(: filter-arity : (℘ W) Integer → (℘ W))
(define (filter-arity Ws k)
  (for/set: : (℘ W) ([W (in-set Ws)] #:when (= k (length W)))
    W))

(: guard-arity : (Listof (℘ W)) Integer → (Option W))
(define (guard-arity Ws k)
  (let loop ([Ws : (Listof (℘ W)) Ws] [rev-Vs : W '()])
    (match Ws
      ['() (reverse rev-Vs)]
      [(cons W Ws*)
       (match (filter-arity W k)
         [(? set-empty?) #f]
         [(? values W)
          (let ([V^ (for/V^ ([Wᵢ (in-set W)]) (car Wᵢ))])
            (loop Ws* (cons V^ rev-Vs)))])])))

(: ?alloc : (Listof A) (℘ W) Σ → (Option Σ))
(define (?alloc As Ws Σ₀)
  (match (filter-arity Ws (length As))
    [(? set-empty?) #f]
    [Ws (for/fold ([Σ : Σ Σ₀]) ([W (in-set Ws)])
          (foldl ⊔ Σ As W))]))

(: Fmls-arity : Fmls → (U Natural arity-at-least))
(define (Fmls-arity fmls)
  (match-define (Fmls xs ?r) fmls)
  (if ?r (arity-at-least (length xs)) (length xs)))

(: non-havocking? : Symbol → Boolean)
(define (non-havocking? o)
  (case o
    [(+ - * / equal? eq? eqv? assq vector vector-set! vector-ref not
        collect-garbage symbol? memq memv null? pair? <= < > >= = vector? vector-length member
        list make-vector length append void cdddr cadddr cadr caddr cddr)
     #t]
    [else #f]))

(: make-havoc-args : Fmls → W)
(define (make-havoc-args fmls)
  (match-define (Fmls xs ?z) fmls)
  ;; FIXME: restore soundness for rest arg: allocate opaque list directly
  (make-list (length xs) {set (Opq)}))

(: summarize : $ Σ G → (Values S (℘ L)))
(define (summarize $ Σ G)
  (define loop-entries (cycles G (L -2 -2)))
  (define (loop-entry? [V₀ : V]) : Boolean
    (define seen : (Mutable-HashTable A #t) (make-hash))
    (let loop ([V : V V₀])
      (match V
        [(Opq) #t]
        [(Lam _ _ L) (∋ loop-entries L)]
        [(Str 'terminating-function (list A))
         (cond [(hash-has-key? seen A) #f]
               [else
                (hash-set! seen A #t)
                (set-ormap loop (hash-ref Σ A mk-∅))])]
        [_ #f])))
  (define flows
    ;; for each `(Eₕ E ...)ᴸ`, map `L` to `eval(Eₕ)`
    (for*/fold ([acc : (Immutable-HashTable L V^) (hash)])
               ([Eᵢ (in-hash-keys $)]
                #:when (App? Eᵢ)
                [Lᵢ (in-value (App-_2 Eᵢ))]
                [Wₕ (in-set (hash-ref $ (App-_0 Eᵢ)))]
                #:when (= 1 (length Wₕ))
                [Vₕ (in-set (car Wₕ))])
      (hash-update acc Lᵢ (λ ([ans : V^]) (set-add ans Vₕ)) mk-∅)))
  (define app-tags
    (for/hash : S ([(L Vs) (in-hash flows)])
      (values L
              (for/set: : (℘ (U 'lam 'term/c #f)) ([V (in-set Vs)])
                (match V
                  [(? Lam?) 'lam]
                  [(Str 'terminating-function _) 'term/c]
                  [_ #f])))))

  (define loop-entry-apps
    (for/set: : (℘ L) ([(L Vs) (in-hash flows)]
                       #:when (for/or : Boolean ([V (in-set Vs)])
                                (loop-entry? V)))
      L))
  
  (values app-tags loop-entry-apps))

(define -Void {set (list {set (Base (void))})})
