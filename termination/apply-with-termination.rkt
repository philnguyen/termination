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

(struct Record ([last-examined-args : (Listof Any)]
                [sc-graphs : (Listof SC-Graph)])
  #:transparent)

(define hash-procedure! : (Procedure → Integer)
  (let ([procedure-hashes : (HashTable Procedure Integer) (make-hasheq)]
        [next-id : Integer 0]
        [MAX-PROCEDURE-COUNT 1024])
    (λ (f)
      (hash-ref! procedure-hashes f
                 (λ () (begin0 next-id (set! next-id (modulo (+ 1 next-id) MAX-PROCEDURE-COUNT))))))))

;; A Call-Stack maps each function to the prefix that result in it, modulo loops
;; Conceptually, it is an associated list of ⟨function, prefix⟩ that supports constant-time lookup
;; For most functions, the prefix is paired with `#f`
;; For functions that are detected as loop entries, the prefix is paired with `⟨n, R⟩`,
;; where `n` counts up the iteration it's been called, and `R` is the size-change graph.
(define-type Call-Stack (Immutable-HashTable Integer (Pairof Call-Stack (Option (Pairof Positive-Integer Record)))))
(define mt-call-stack : Call-Stack (hasheq))
(define-parameter call-stack : Call-Stack mt-call-stack)

;; The empty call-stack is absused as a "not checking" flag.
;; When termination checking starts, it always pushes to the call-stack.
(define (divergence-ok?) (eq? mt-call-stack (call-stack)))

(: apply/termination (∀ (X Y) (X * → Y) X * → Y))
;; Mark size-change progress before executing the body
(define (apply/termination f . xs)
  (define cs (call-stack))
  (define f:hash (hash-procedure! f))
  (define-values (cs* rec*)
    (match (hash-ref cs f:hash #f)
      ;; Function has previous been on the call stack. It's a loop entry
      [(cons cs₀ ?rec₀)
       (values
        cs₀
        (match ?rec₀
          ;; At the `n₀`th iteration with previous record `r₀`
          [(cons n₀ r₀)
           (define n (add1 n₀))
           ;; If the `n`th iteration is a power of 2, guard against size-change violation
           ;; otherwise use old record
           (define r (if (zero? (unsafe-fxand n n₀)) (update-record r₀ f:hash xs) r₀))
           (cons n r)]
          ;; No previous record. This is the 2nd iteration.
          [_ (cons 1 (Record xs '()))]))]
      ;; Function is not a loop entry
      [_ (values cs #f)]))
  ;; Proceed with current function pushed on stack
  (with-call-stack (hash-set cs* f:hash (cons cs* rec*))
    (apply f xs)))

(: update-record : Record Integer (Listof Any) → Record)
;; Update function `f`'s call record, accumulating observed ways in which it transitions to itself
(define (update-record r₀ f:hash xs)
  (match-define (Record xs₀ Gs₀) r₀)
  (define G (make-sc-graph xs₀ xs))
  (match (find-sc-violation G Gs₀)
    [(? values G-err) (err G-err G f:hash xs₀ xs)]
    [_ (Record xs (cons G Gs₀))]))

(: err : SC-Graph SC-Graph Integer (Listof Any) (Listof Any) → Nothing)
(define (err G-err G f:hash xs₀ xs)
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
           (car ((inst argmax (Pairof Integer (Pairof Call-Stack Any)))
                 (match-lambda
                   [(cons p h) (hash-count (car h))])
                 (hash->list cs))))
         (cons (format "  * ~a" top) (go (car (hash-ref cs top))))])))
  (define lines
    `(,(format "Recursive call to code point `~a` has no obvious descendence on any argument" f:hash)
      "- Preceding call:"  ,@(args->lines xs₀)
      "- Subsequent call:" ,@(args->lines xs)
      "New graph:" ,@(graph->lines G)
      "Size-change violating graph:" ,@(graph->lines G-err)
      "Call stack:" ,@(stack->lines)))
  (error 'possible-non-termination (string-join lines "\n")))
