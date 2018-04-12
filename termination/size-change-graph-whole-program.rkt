#lang typed/racket/base

(require racket/match
         racket/set
         racket/list
         racket/string
         racket/unsafe/ops
         typed/racket/unsafe
         syntax/parse/define
         "flattened-parameter.rkt")

;; `unsafe-provide` to get around contracts messing with functions as hash-table keys
(unsafe-provide mt-call-stack
                divergence-ok?
                update-stack
                (rename-out [with-<? with-custom-<]))

;; A size-change graph tracks how a function calls itself,
;; where each edge denotes a "must" non-ascendence between argument indices
(define-type Size-Change-Graph (Immutable-HashTable (Pairof Integer Integer) Dec))

;; Transition between values based on some well-founded partial order
;; - `↓` is definite descendence
;; - '↧` is definite non-ascendence
;; - `#f` is conservative "don't know"
(define-type Dec (U '↓ '↧))

(struct Call-Record ([most-recent-args : (Listof Any)]
                     [accumulated-change-graph : Size-Change-Graph])
  #:transparent)

(define-type Call-Stack (Immutable-HashTable Procedure (Pairof Call-Stack (Option (Pairof Positive-Integer Call-Record)))))
(define mt-call-stack : Call-Stack (hasheq))

;; The empty call-stack is absused as a "not checking" flag.
;; When termination checking starts, it always pushes to the call-stack.
(define (divergence-ok? [st : Call-Stack]) (eq? mt-call-stack st))

(: update-stack (∀ (X Y) Call-Stack (X * → Y) X * → Call-Stack))
(define (update-stack st f . xs)
  (define-values (st* entry*)
    (match (hash-ref st f #f)
      [(cons st₀ ?rec)
       (match ?rec
         [(cons n₀ r₀)
          (define n (add1 n₀))
          (define r (if (zero? (unsafe-fxand n n₀)) (update-Record r₀ f xs) r₀))
          (values st₀ (cons n r))]
         [_
          (values st₀ (cons 1 (Call-Record xs (init-size-change-graph (length xs)))))])]
      [_ (values st #f)]))
  (hash-set st* f (cons st* entry*)))

(: update-Record : Call-Record Procedure (Listof Any) → Call-Record)
;; Update function `f`'s call record, accumulating observed ways in which it transitions to itself
(define (update-Record r₀ f xs)
  (match-define (Call-Record xs₀ G₀) r₀)
  (define G (mk-graph xs₀ xs))
  (cond [(strictly-descending? G)
         (define G* (concat-graph G₀ G))
         (cond [(strictly-descending? G*) (Call-Record xs G*)]
               [else (err G* f xs₀ xs)])]
        [else (err G f xs₀ xs)]))

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
          (format "  - ~a ~a ~a" (car edge) ↝ (cdr edge)))))
  (error 'possible-non-termination (string-join lines "\n")))

(: strictly-descending? : Size-Change-Graph → Boolean)
(define (strictly-descending? G) (for/or ([d (in-hash-values G)]) (eq? d '↓)))

(: init-size-change-graph : Index → Size-Change-Graph)
;; Initial size-change graph, where each argument have strictly descended from "infinity"
(define init-size-change-graph
  (let ([cache : (Mutable-HashTable Index Size-Change-Graph) (make-hasheq)])
    (λ (n)
      (hash-ref! cache n
                 (λ () (for/hash : Size-Change-Graph ([i (in-range n)])
                         (values (cons i i) '↓)))))))

(: concat-graph : Size-Change-Graph Size-Change-Graph → Size-Change-Graph)
(define (concat-graph G₁ G₂)
  (for*/fold ([G* : Size-Change-Graph (hash)])
             ([(edge₁ ↝₁) (in-hash G₁)]
              [i (in-value (cdr edge₁))]
              [(edge₂ ↝₂) (in-hash G₂)]
              #:when (eq? i (car edge₂)))
    (hash-update G* (cons (car edge₁) (cdr edge₂))
                 (λ ([↝₀ : Dec]) (Dec-best ↝₀ ↝₁ ↝₂))
                 (λ () '↧))))

(: mk-graph : (Listof Any) (Listof Any) → Size-Change-Graph)
;; Make size-change graph from comparing old and new argument lists
(define (mk-graph xs₀ xs₁)
  (define cmp (let ([≺ (<?)]) (λ (x y) (if (equal? x y) '↧ (and (≺ y x) '↓)))))
  (for*/hash : Size-Change-Graph ([(v₀ i₀) (in-indexed xs₀)]
                                  [(v₁ i₁) (in-indexed xs₁)]
                                  [?↓ (in-value (cmp v₀ v₁))] #:when ?↓)
    (values (cons i₀ i₁) ?↓)))

(: <?:default : Any Any → Boolean)
;; Simple default implementation of well-founded strict partial order on data
(define (<?:default x y)
  (cond [(integer? y) (and (integer? x) (< -1 x y))]
        [(pair? y) (or (equal? x (car y))
                       (equal? x (cdr y))
                       (<?:default x (car y))
                       (<?:default x (cdr y)))]
        [else #f]))

(define-parameter <? : (Any Any → Boolean) <?:default)

(define Dec-best : (Dec * → Dec)
  (match-lambda*
   [(list '↧ ...) '↧]
   [_ '↓]))
