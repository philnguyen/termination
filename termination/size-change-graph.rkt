;; `no-check` to disable conflicting contracts with (hasheq) when imported by untyped
;; Module works fine on its own. Turn on manually to check sometimes.
#lang typed/racket/base/no-check

(require racket/match
         racket/set
         racket/list
         racket/string)

(provide update-Call-Histories
         call-histories
         enforcing-termination?
         custom-<?
         check-interval)

;; A size-change graph tracks how a function call "transitions" to itself,
;; where each edge denotes a "must" non-ascendence
(define-type Size-Change-Graph (Immutable-HashTable (Pairof Integer Integer) Dec))
(define-type Dec (U '↓ '↧))
(define-type ?Dec (Option Dec)) ; ↓ ⊑ ↧ ⊑ #f

;; A function's call history tracks:
;; - last-arguments: the most recent argument list in the call stack
;; - change-graphs: observed ways in which the function transitions to itself
;; - count-down: how many calls to this function before it is checked again
(struct Call-History ([count-down : Positive-Integer]
                      [last-arguments : (Vectorof Any)]
                      [change-graphs : (Setof Size-Change-Graph)]) #:transparent)

;; Call-Histories is a table tracking all function calls in the current call-chain
;; starting where a termination-contract is triggered
(define-type Call-Histories (Immutable-HashTable Procedure Call-History))
(define Empty-Call-Histories ((inst hasheq Procedure Call-History)))

(define call-histories : (Parameterof Call-Histories) (make-parameter Empty-Call-Histories))
(define check-interval : (Parameterof Positive-Integer) (make-parameter 1))
(define custom-<? : (Parameterof (Any Any → Boolean)) (make-parameter (λ _ #f)))

;; The empty call-histories is absused as a "not checking" flag.
;; When termination checking starts, it always pushs an entry to the table.
(define (enforcing-termination?) (not (hash-empty? (call-histories)))) 

(: update-Call-Histories : Call-Histories Procedure (Vectorof Any) → Call-Histories)
;; Update function `f`'s call history, accumulating observed ways in which it transitions to itself
(define (update-Call-Histories M f xs)
  (match (hash-ref M f #f)
    [(Call-History n₀ xs₀ Gs₀)
     (match (sub1 n₀)
       [(? positive? n) (hash-set M f (Call-History n xs₀ Gs₀))]
       [0 (define G (mk-graph xs₀ xs))
          (define Gs* (refl-trans (set-add Gs₀ G)))
          (ensure-size-change-termination Gs* f xs₀ xs) 
          (hash-set M f (Call-History (check-interval) xs Gs*))])]
    [#f
     ;; First observed arguements. Assume they have strictly descended from "infinity"
     (define G₀ (for/hash : Size-Change-Graph ([i (in-range (vector-length xs))])
                  (values (cons i i) '↓)))
     (hash-set M f (Call-History (check-interval) xs (refl-trans {set G₀})))]))

(: ensure-size-change-termination : (Setof Size-Change-Graph) Procedure (Vectorof Any) (Vectorof Any) → Void)
(define (ensure-size-change-termination Gs f xs₀ xs)
  (match (size-change-violation? Gs)
    [(? values G)
     (define lines
       `(,(format "Recursive call to `~a` has no obvious descendence on any argument" f)
         ,(format "- Preceding call:")
         ,@(for/list : (Listof String) ([(x i) (in-indexed xs₀)])
             (format "  * arg ~a: ~a" i x))
         ,(format "- Subsequent call:")
         ,@(for/list : (Listof String) ([(x i) (in-indexed xs)])
             (format "  * arg ~a: ~a" i x))
         ,"Size-change graphs:"
         ,@(append-map
            (λ ([G* : Size-Change-Graph])
              (cons
               (if (equal? G* G) (format "- Troublesome graph:") (format "- Graph:"))
               (map
                (λ ([edge : (Pairof (Pairof Integer Integer) Dec)])
                  (match-define (cons (cons src tgt) ↝) edge)
                  (format "  - ~a ~a ~a" src ↝ tgt))
                (hash->list G))))
            (set->list Gs))))
     (error 'possible-non-termination (string-join lines "\n"))]
    [_ (void)]))

(: size-change-violation? : (Setof Size-Change-Graph) → (Option Size-Change-Graph))
;; Step 2 in Algorithm in section 3.2
;; Search for one possible transition whose infinite repetition would decrease no argument
(define (size-change-violation? Gs)
  (for/or : (Option Size-Change-Graph) ([G : Size-Change-Graph (in-set Gs)])
    (and (equal? G (compose-graph G G))
         (not (for/or : Boolean ([(edge ↝) (in-hash G)])
                (match* (edge ↝)
                  [((cons i i) '↓) #t]
                  [(_ _) #f])))
         G)))

(: refl-trans : (Setof Size-Change-Graph) → (Setof Size-Change-Graph))
;; Step 1 in Algorithm in section 3.2
(define (refl-trans Gs)
  (define (trans [Gs : (Setof Size-Change-Graph)])
    (set-union Gs
               (for*/set: : (Setof Size-Change-Graph) ([G₁ : Size-Change-Graph (in-set Gs)]
                                                       [G₂ : Size-Change-Graph (in-set Gs)])
                 (compose-graph G₁ G₂))))
  (let fix ([Gs : (Setof Size-Change-Graph) Gs])
    (define Gs* (trans Gs))
    (if (equal? Gs* Gs) Gs (fix Gs*))))

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
    (hash-update G* (cons s₁ t₂) (λ ([↝₀ : Dec]) (Dec-best ↝₀ (Dec-best ↝₁ ↝₂))) (λ () '↧))))

(: mk-graph : (Vectorof Any) (Vectorof Any) → Size-Change-Graph)
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
(define cmp
  (match-lambda**
    [(V V) '↧]
    [((? exact-nonnegative-integer? m) (? exact-nonnegative-integer? n)) #:when (> m n) '↓]
    [((cons V₁ V₂) V) #:when (or (cmp V₁ V) (cmp V₂ V)) '↓]
    [(V₁ V₂) #:when ((custom-<?) V₁ V₂) '↓]
    [(_ _) #f]))

(define Dec-best : (Dec Dec → Dec)
  (match-lambda**
   [('↧ '↧) '↧]
   [(_ _) '↓]))
