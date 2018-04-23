#lang typed/racket/base

;; `unsafe-provide` to get around contracts messing with function identities (e.g. in `hasheq`)
(require typed/racket/unsafe)
(unsafe-provide apply/termination
                divergence-ok?
                with-<?)

(require racket/match
         racket/list
         racket/string
         racket/set
         racket/unsafe/ops
         "flattened-parameter.rkt"
         "size-change-graph.rkt")

(define-type Record (Pairof (Listof Any) Local-Stack))
(define-type Local-Record (Pairof (Listof Any) SC-Graph))
(define-type Arg-Map (Immutable-HashTable Integer Any))
(define gen (gensym 'generic))

(define-type Call-Stack (Immutable-HashTable Procedure (Pairof Call-Stack (Option (Pairof Positive-Integer Record)))))
(define-type Local-Stack (Immutable-HashTable Arg-Map Local-Record))
(define mt-call-stack : Call-Stack (hasheq))
(define mt-local-stack : Local-Stack (hash))
(define-parameter call-stack : Call-Stack mt-call-stack)

;; The empty call-stack is absused as a "not checking" flag.
;; When termination checking starts, it always pushes to the call-stack.
(define (divergence-ok?) (eq? mt-call-stack (call-stack)))

(: apply/termination (∀ (X Y) (X * → Y) X * → Y))
;; Mark size-change progress before executing the body
(define (apply/termination f . xs)
  (define cs (call-stack))
  (define rec*
    (match (hash-ref cs f #f)
      [(cons cs₀ ?rec₀)
       (match ?rec₀
         [(cons n₀ r₀)
          (define n (add1 n₀))
          (define r (if (zero? (unsafe-fxand n n₀)) (update-local-stack r₀ f xs) r₀))
          (cons n r)]
         [_ (cons 1 (cons xs mt-local-stack))])]
      [_ #f]))
  (with-call-stack (hash-set cs f (cons cs rec*))
    (apply f xs)))

(: update-local-stack : Record Procedure (Listof Any) → Record)
;; Update function `f`'s call record, accumulating observed ways in which it transitions to itself
(define (update-local-stack r₀ f xs)
  (match-define (cons xs₀ st₀) r₀)
  (define xs* (sub-args xs₀ xs))
  (define G*
    (match (hash-ref st₀ xs* #f)
      [(cons xs₁ G₁)
       (define G (make-sc-graph xs₁ xs))
       (or (and (strictly-descending? G)
                (let ([G* (concat-graph G₁ G)])
                  (and (strictly-descending? G*)
                       G*)))
           (err G₁ G (cons f xs*) xs₁ xs))]
      [_ (init-sc-graph (length xs))]))
  (cons xs₀ (hash-set st₀ xs* (cons xs G*))))

(: err : SC-Graph SC-Graph (Pairof Procedure Arg-Map) (Listof Any) (Listof Any) → Nothing)
(define (err G₀ G f xs₀ xs)
  (define (graph->lines [G : SC-Graph])
    (for/list : (Listof String) ([(edge ↝) (in-hash G)])
      (format "  * ~a ~a ~a" (car edge) ↝ (cdr edge))))
  (define (args->lines [xs : (Listof Any)])
    (for/list : (Listof String) ([(x i) (in-indexed xs)])
      (format "  * arg ~a: ~a" i x)))
  (define (stack->lines)
    (let go : (Listof String) ([cs : Call-Stack (call-stack)])
      (cond
        [(hash-empty? cs) '()]
        [else
         (define top
           (car ((inst argmax (Pairof Procedure (Pairof Call-Stack Any)))
                 (match-lambda
                   [(cons p h) (hash-count (car h))])
                 (hash->list cs))))
         (cons (format "  * ~a" top) (go (car (hash-ref cs top))))])))
  (define lines
    `(,(format "Recursive call to `~a` has no obvious descendence on any argument" f)
      "- Preceding call:"  ,@(args->lines xs₀)
      "- Subsequent call:" ,@(args->lines xs)
      "Initial graph:" ,@(graph->lines G₀)
      "Step graph:"    ,@(graph->lines G)
      "Call stack:" ,@(stack->lines)))
  (error 'possible-non-termination (string-join lines "\n")))

(: sub-args : (Listof Any) (Listof Any) → Arg-Map)
(define (sub-args xs₀ xs₁)
  (for/hash : Arg-Map ([x₁ (in-list xs₁)]
                       [i (in-naturals)]
                       #:when (structurally-<? x₁ xs₀))
    (values i x₁)))

(: structurally-<? : Any Any → Boolean)
(define (structurally-<? x y)
  (cond [(and (integer? x) (integer? y)) (< -1 x y)]
        [else
         (let go ([y y])
           (or (and (pair? y)
                    (or (eq? x (car y))
                        (eq? x (cdr y))
                        (go (car y))
                        (go (cdr y))))
               (and (hash? y)
                    (for/or : Boolean ([y* (in-hash-values y)])
                      (or (eq? x y*) (go y*))))))]))
