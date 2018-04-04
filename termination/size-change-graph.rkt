#lang typed/racket/base

(require racket/match
         racket/set
         racket/list
         racket/string
         typed/racket/unsafe
         syntax/parse/define)

;; `unsafe-provide` to get around contracts messing with functions as hash-table keys
(unsafe-provide enforcing-termination?
                custom-<?
                with-call-monitored)

(define-simple-macro (define-parameter x:id (~literal :) T e)
  (define x ((inst make-parameter T) e)))

;; A size-change graph tracks how a function call "transitions" to itself,
;; where each edge denotes a "must" non-ascendence
(define-type Size-Change-Graph (Immutable-HashTable (Pairof Integer Integer) Dec))
(define-type Dec (U '↓ '↧))
(define-type ?Dec (Option Dec)) ; ↓ ⊑ ↧ ⊑ #f

(struct Call-History ([#|most recent call       |# last-arguments : (Listof Any)]
                      [#|accumulated size change|# change-graph : Size-Change-Graph])
  #:transparent)

;; Call-Histories is a table tracking all function calls in the current call-chain
;; starting where a termination-contract is triggered
(define-type Call-Histories (Immutable-HashTable Procedure Call-History))

;; Call-stack is (conceptually) linked list of functions that supports constant time `memq`
(define-type Call-Stack (Immutable-HashTable Procedure Call-Stack))

(define-parameter call-histories : Call-Histories (hasheq))
(define-parameter call-stack : Call-Stack (hasheq))
(define-parameter count-downs : (Immutable-HashTable Procedure Positive-Integer) (hasheq))
(define-parameter count-down-bases : (Immutable-HashTable Procedure Natural) (hasheq))
(define-parameter custom-<? : (Any Any → Boolean) (λ _ #f))

;; The empty call-stack is absused as a "not checking" flag.
;; When termination checking starts, it always pushes to the call-stack.
(define (enforcing-termination?) (not (hash-empty? (call-stack))))

(: with-call-monitored (∀ (X) Procedure (Listof Any) (→ X) → X))
;; Mark size-change progress before executing the body
(define (with-call-monitored f xs exec)
  (define cs (call-stack))
  (match (hash-ref cs f #f)
    [(? values cs₀) ; looped
     (define cd (count-downs))
     (define cdb (count-down-bases))
     (match (sub1 (hash-ref cd f (λ () 0)))
       [(? positive? n) ; Spare more iterations
        (parameterize ([call-stack (hash-set cs₀ f cs₀)]
                       [count-downs (hash-set cd f n)])
          (exec))]
       [_
        (define b (cond [(hash-ref cdb f #f) => add1] [else 0]))
        (define n (expt 2 b))
        (parameterize ([call-stack (hash-set cs₀ f cs₀)]
                       [count-down-bases (hash-set cdb f b)]
                       [count-downs (hash-set cd f n)]
                       [call-histories (update-Call-Histories (call-histories) f xs)])
          (exec))])]
    [_
     (parameterize ([call-stack (hash-set cs f cs)])
       (exec))]))

(: update-Call-Histories : Call-Histories Procedure (Listof Any) → Call-Histories)
;; Update function `f`'s call history, accumulating observed ways in which it transitions to itself
(define (update-Call-Histories M f xs)
  (define new-history
    (match (hash-ref M f #f)
      [(Call-History xs₀ G₀)
       (define G (mk-graph xs₀ xs))
       (cond [(strictly-descending? G)
              (define G* (compose-graph G₀ G))
              (cond [(strictly-descending? G*) (Call-History xs G*)]
                    [else (err G* f xs₀ xs)])]
             [else (err G f xs₀ xs)])]
      [#f (Call-History xs (init-size-change-graph (length xs)))]))
  (hash-set M f new-history))

(: err : Size-Change-Graph Procedure (Listof Any) (Listof Any) → Nothing)
(define (err G f xs₀ xs)
  (define lines
    `(,(format "Recursive call to `~a` has no obvious descendence on any argument" f)
      ,(format "- Preceding call:")
      ,@(for/list : (Listof String) ([(x i) (in-indexed xs₀)])
          (format "  * arg ~a: ~a" i x))
      ,(format "- Subsequent call:")
      ,@(for/list : (Listof String) ([(x i) (in-indexed xs)])
          (format "  * arg ~a: ~a" i x))
      ,"Accumulated size-change graph:"
      ,@(for/list : (Listof String) ([(edge ↝) (in-hash G)])
          (match-define (cons src tgt) edge)
          (format "  - ~a ~a ~a" src ↝ tgt))))
  (error 'possible-non-termination (string-join lines "\n")))

(: strictly-descending? : Size-Change-Graph → Boolean)
(define (strictly-descending? G) (for/or : Boolean ([d (in-hash-values G)]) (eq? d '↓)))

(: init-size-change-graph : Index → Size-Change-Graph)
;; Initial size-change graph, where each argument have strictly descended from "infinity"
(define init-size-change-graph
  (let ([cache : (Mutable-HashTable Index Size-Change-Graph) (make-hasheq)])
    (λ (n)
      (hash-ref! cache n
                 (λ () (for/hash : Size-Change-Graph ([i (in-range n)])
                         (values (cons i i) '↓)))))))

(: compose-graph : Size-Change-Graph Size-Change-Graph → Size-Change-Graph)
(define (compose-graph G₁ G₂)
  (for*/fold ([G* : Size-Change-Graph (hash)])
             ([(edge₁ ↝₁) (in-hash G₁)]
              [(edge₂ ↝₂) (in-hash G₂)]
              [s₁ (in-value (car edge₁))]
              [t₁ (in-value (cdr edge₁))]
              [s₂ (in-value (car edge₂))]
              [t₂ (in-value (cdr edge₂))]
              #:when (equal? t₁ s₂))
    (hash-update G* (cons s₁ t₂) (λ ([↝₀ : Dec]) (Dec-best ↝₀ ↝₁ ↝₂)) (λ () '↧))))

(: mk-graph : (Listof Any) (Listof Any) → Size-Change-Graph)
;; Make size-change graph from comparing old and new argument lists
(define (mk-graph xs₀ xs₁)
  (for*/hash : Size-Change-Graph ([(v₀ i₀) (in-indexed xs₀)]
                                  [(v₁ i₁) (in-indexed xs₁)]
                                  [?↓ (in-value (cmp v₀ v₁))]
                                  #:when ?↓)
    (values (cons i₀ i₁) ?↓)))

(: cmp : Any Any → ?Dec)
;; Judge the transition between former and latter value based on some well-founded partial order
;; - `↓` is definite descendence
;; - '↧` is definite non-ascendence
;; - `#f` is conservative "don't know"
(define (cmp x y)
  (cond [(equal? x y) '↧]
        [(and (integer? x) (integer? y) (> x y -1)) '↓]
        [(and (pair? x) (or (cmp (car x) y) (cmp (cdr x) y))) '↓]
        [else (and ((custom-<?) x y) '↓)]))

(define Dec-best : (Dec * → Dec)
  (match-lambda*
   [(list '↧ ...) '↧]
   [_ '↓]))
