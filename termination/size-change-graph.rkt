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
                check-interval
                with-call-monitored)

(define-simple-macro (define-parameter x:id (~literal :) T e)
  (define x ((inst make-parameter T) e)))

;; A size-change graph tracks how a function call "transitions" to itself,
;; where each edge denotes a "must" non-ascendence
(define-type Size-Change-Graph (Immutable-HashTable (Pairof Integer Integer) Dec))
(define-type Dec (U '↓ '↧))
(define-type ?Dec (Option Dec)) ; ↓ ⊑ ↧ ⊑ #f

;; A function's call history tracks:
;; - last-arguments: the most recent argument list in the call stack
;; - change-graphs: observed ways in which the function transitions to itself
(struct Call-History ([last-arguments : (Listof Any)]
                      [change-graph : Size-Change-Graph]) #:transparent)

;; Call-Histories is a table tracking all function calls in the current call-chain
;; starting where a termination-contract is triggered
(define-type Call-Histories (Immutable-HashTable Procedure Call-History))

;; Basically a linked list that supports constant-time `memq`
(define-type Call-Stack (Immutable-HashTable Procedure Call-Stack))

(define-parameter call-histories : Call-Histories (hasheq))
(define-parameter call-stack : Call-Stack (hasheq))
(define-parameter count-downs : (Immutable-HashTable Procedure Positive-Integer) (hasheq))
(define-parameter count-down-bases : (Immutable-HashTable Procedure Natural) (hasheq))
(define-parameter check-interval : Positive-Index 1)
(define-parameter custom-<? : (Any Any → Boolean) (λ _ #f))

;; The empty call-histories is absused as a "not checking" flag.
;; When termination checking starts, it always pushs an entry to the table.
(define (enforcing-termination?) (not (hash-empty? (call-stack))))

(: with-call-monitored (∀ (X) Procedure (Listof Any) (→ X) → X))
(define (with-call-monitored f xs exec)
  (define cs (call-stack))
  (match (hash-ref cs f #f)
    [(? values cs₀) ; looped
     (define cd (count-downs))
     (define cdb (count-down-bases))
     (match (sub1 (hash-ref cd f (λ () 0)))
       ;; Spare iterations
       [(? positive? n)
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
       (if (strictly-descending? G)
           (let ([G* (compose-graph G₀ G)])
             (if (strictly-descending? G*)
                 (Call-History xs G*)
                 (err G* f xs₀ xs)))
           (err G f xs₀ xs))]
      [#f ; First observed arguements. Assume they have strictly descended from "infinity"
       (define G₀ (for/hash : Size-Change-Graph ([i (in-range (length xs))])
                    (values (cons i i) '↓)))
       (Call-History xs G₀)]))
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
      ,@(map
         (λ ([edge : (Pairof (Pairof Integer Integer) Dec)])
           (match-define (cons (cons src tgt) ↝) edge)
           (format "  - ~a ~a ~a" src ↝ tgt))
         (hash->list G))))
  (error 'possible-non-termination (string-join lines "\n")))

(: strictly-descending? : Size-Change-Graph → Boolean)
(define (strictly-descending? G)
  (for/or : Boolean ([d (in-hash-values G)]) (eq? d '↓)))

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
