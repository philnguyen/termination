#lang typed/racket/base

;; `unsafe-provide` to get around contracts messing with function identities (e.g. in `hasheq`)
(require typed/racket/unsafe)
(unsafe-provide apply/termination
                divergence-ok?
                with-<?)

(require racket/match
         racket/list
         racket/string
         racket/unsafe/ops
         "flattened-parameter.rkt"
         "size-change-graph.rkt")

(struct Record ([last-examined-args : (Listof Any)] [last-sc-graph : SC-Graph]) #:transparent)

(define-type Call-Stack (Immutable-HashTable Procedure (Pairof Call-Stack (Option (Pairof Positive-Integer Record)))))
(define mt-call-stack : Call-Stack (hasheq))
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
          (define r (if (zero? (unsafe-fxand n n₀)) (update-sc-graph r₀ f xs) r₀))
          (cons n r)]
         [_ (cons 1 (Record xs (init-sc-graph (length xs))))])]
      [_ #f]))
  (with-call-stack (hash-set cs f (cons cs rec*))
    (apply f xs)))

(: update-sc-graph : Record Procedure (Listof Any) → Record)
;; Update function `f`'s call record, accumulating observed ways in which it transitions to itself
(define (update-sc-graph r₀ f xs)
  (match-define (Record xs₀ G₀) r₀)
  (define G (make-sc-graph xs₀ xs))
  (or (and (strictly-descending? G)
           (let ([G* (concat-graph G₀ G)])
             (and (strictly-descending? G*) (Record xs G*))))
      (err G₀ G f xs₀ xs)))

(: err : SC-Graph SC-Graph Procedure (Listof Any) (Listof Any) → Nothing)
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
