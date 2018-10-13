#lang typed/racket/base

(provide (all-defined-out))

(require racket/match
         racket/set
         racket/list
         set-extras
         bnf
         unreachable)

(#|Modules           |# M . ::= . (M (Listof D) (℘ Symbol)))
(#|Module-Level Forms|# D . ::= . E
                                  (Define-Values (Listof Symbol) E))
(#|Expressions       |# E . ::= . (Lam Fmls E L)
                                  (Case-Lam (Listof Lam))
                                  (If E E E)
                                  (Begin (Listof E))
                                  (Begin0 E E)
                                  (Let (Listof Bnd) E)
                                  (Letrec (Listof Bnd) E)
                                  (Set! (U Top-Ref Lex-Ref) E)
                                  (Wcm E E E)
                                  (App E (Listof E) L)
                                  (Top-Ref Symbol)
                                  (Lex-Ref Symbol)
                                  (Prim Symbol)
                                  (Base Any)
                                  (Opq))
(#|Addresses         |# A . ::= . Lex-Ref
                                  Top-Ref
                                  (A:Var Symbol)
                                  (A:Field StrTag Integer L)
                                  (A:Vect-Field Integer L)
                                  (A:Vectof-Field L)
                                  (A:Havoc))
(#|Analysis Results  |# R . ≜   . (℘ W))
(#|Stores            |# Σ . ≜   . (Immutable-HashTable A V^))
(#|Caches            |# $ . ≜   . (Immutable-HashTable E R))
(#|Value             |# V . ::= . Base
                                  Prim
                                  Lam
                                  Case-Lam
                                  (Str StrTag (Listof A))
                                  Opq
                                  (Fo-Opq)
                                  (Vect (Listof A))
                                  (Vectof A))
(#|Parameter Lists   |# Fmls . ::= . (Fmls [init : (Listof Symbol)] [rest : (Option Symbol)]))
(#|Struct Tags       |# StrTag . ≜ . Symbol)
(#|Abstract Values   |# V^ . ≜ . (℘ V))
(#|Bindings          |# Bnd . ::= . (Bnd (Listof Symbol) E))
(#|Value Lists       |# W . ≜ . (Listof V^))
(#|Source Locations  |# L . ::= . (L [line : Integer] [col : Integer]))
(#|Analysis Summaries|# S . ≜   . (Immutable-HashTable L (℘ (U 'lam 'term/c #f))))
(#|Call Graphs       |# G . ≜   . (Immutable-HashTable L (℘ L)))

(: cache-ref : $ E → R)
(define (cache-ref $ E) (hash-ref $ E (λ () ∅)))

(define env-ref (inst hash-ref Symbol A))
(define env-set (inst hash-set Symbol A))


(define-syntax-rule (for/R (clause ...) body ...)
  (for/fold ([acc : R ∅]) (clause ...)
    (∪ acc (let () body ...))))

(define-syntax-rule (for/V^ (clause ...) body ...)
  (for/fold ([acc : V^ ∅]) (clause ...)
    (∪ acc (let () body ...))))

(: $⊔ : E R $ → $)
(define ($⊔ E R* $) (hash-update $ E (λ ([R₀ : R]) (∪ R₀ R*)) (λ () ∅)))

(: ⊔ : A V^ Σ → Σ)
(define (⊔ A V Σ) (hash-update Σ A (λ ([V₀ : V^]) (∪ V₀ V)) mk-∅))

(: ⊔¹ : A V Σ → Σ)
(define (⊔¹ A V Σ) (hash-update Σ A (λ ([V₀ : V^]) (set-add V₀ V)) mk-∅))

(: G⊔¹ : L L G → G)
(define (G⊔¹ src tgt G)
  (hash-update G src (λ ([tgts : (℘ L)]) (set-add tgts tgt)) mk-∅))

(: fv : E → (℘ Symbol))
(define fv
  (match-lambda
    [(Lam Fmls E _) (set-subtract (fv E) (Fmls-vars Fmls))]
    [(Case-Lam Es) (apply ∪ ∅eq (map fv Es))]
    [(If E E₁ E₂) (∪ (fv E) (fv E₁) (fv E₂))]
    [(Begin Es) (apply ∪ ∅eq (map fv Es))]
    [(Begin0 E₀ E) (∪ (fv E₀) (fv E))]
    [(Let Bnds E) (define-values (bvs fvs) (vars/fv* Bnds))
                  (∪ fvs (set-subtract (fv E) bvs))]
    [(Letrec Bnds E) (define-values (bvs fvs) (vars/fv* Bnds))
                     (set-subtract (∪ fvs (fv E)) bvs)]
    [(Set! X E) (match X
                  [(Lex-Ref x) (set-add (fv E) x)]
                  [_ (fv E)])]
    [(Wcm Eₖ Eᵥ E) (∪ (fv Eₖ) (fv Eᵥ) (fv E))]
    [(App E Es _) (apply set-union (fv E) (map fv Es))]
    [(Lex-Ref X) {seteq X}]
    [_ ∅eq]))

(: vars/fv* : (Listof Bnd) → (Values (℘ Symbol) (℘ Symbol)))
(define (vars/fv* bnds)
  (for/fold ([bvs : (℘ Symbol) ∅eq] [fvs : (℘ Symbol) ∅eq])
            ([bnd (in-list bnds)])
    (match-define (Bnd xs E) bnd)
    (values (∪ bvs (list->seteq xs)) (∪ fvs (fv E)))))

(: Fmls-vars : Fmls → (℘ Symbol))
(define Fmls-vars
  (match-lambda
    [(Fmls xs ?r) (if ?r (set-add (list->seteq xs) ?r) (list->seteq xs))]))
