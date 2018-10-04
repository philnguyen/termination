#lang typed/racket/base

;; `unsafe-provide` to get around contracts messing with function identities (e.g. in `hasheq`)
(require typed/racket/unsafe)
(unsafe-provide apply/guard/restore
                apply/guard
                divergence-ok?
                with-<?)

(require racket/match
         racket/list
         racket/string
         racket/set
         racket/unsafe/ops
         "size-change-graph.rkt")

(struct Record ([last-examined-args : (Listof Any)]
                [last-sc-graph : (Pairof SC-Graph (Listof SC-Graph))])
  #:mutable
  #:transparent)

;; hack just to see limit
(define-syntax dynamic-wind
  (syntax-rules (λ)
    [(_ (λ () e₁ ...) exec (λ () e₂ ...))
     (let ()
       e₁ ...
       (begin0 (exec)
         e₂ ...))]))

(define call-stack ((inst make-hasheq Procedure (U #t (MPairof Positive-Integer Record)))))

(define (divergence-ok?) (hash-empty? call-stack))

(: apply/guard/restore (∀ (X Y) (X * → Y) X * → Y))
(define (apply/guard/restore f . xs)
  (define (exec) (apply f xs))

  (define (exec/mark-seen)
    (dynamic-wind
      (λ () (hash-set! call-stack f #t))
      exec
      (λ () (hash-remove! call-stack f))))

  (define (exec/mark-first-loop)
    (dynamic-wind
      (λ () (hash-set! call-stack f ((inst mcons Positive-Integer Record) 1 (Record xs (list (init-sc-graph (length xs)))))))
      exec
      (λ () (hash-set! call-stack f #t))))

  (: exec/bump-loop-count : (MPairof Positive-Integer Record) Positive-Integer Positive-Integer → Y)
  (define (exec/bump-loop-count rec n₀ n)
    (dynamic-wind
      (λ () (set-mcar! rec n))
      exec
      (λ () (set-mcar! rec n₀))))

  (: exec/bump-loop-count-and-check : (MPairof Positive-Integer Record) Positive-Integer Positive-Integer → Y)
  (define (exec/bump-loop-count-and-check rec n₀ n)
    (define r (mcdr rec))
    (define xs₀ (Record-last-examined-args r))
    (define Gs₀ (Record-last-sc-graph r))
    (dynamic-wind
      (λ ()
        (define Gs (update-record r f xs))
        (set-mcar! rec n)
        (set-Record-last-examined-args! r xs)
        (set-Record-last-sc-graph! r Gs))
      exec
      (λ ()
        (set-mcar! rec n₀)
        (set-Record-last-examined-args! r xs₀)
        (set-Record-last-sc-graph! r Gs₀))))

  (cond
    ;; `f` detected as loop entry
    [(hash-ref call-stack f #f)
     =>
     (λ (rec)
       (printf "applying ~a to ~a: ~a~n" f xs rec)
       (if (mpair? rec)
           ;; `f` loops back second time onwards
           (let* ([n₀ (mcar rec)]
                  [n (add1 n₀)])
             ;; bump loop count, check against SCT violation at 2ⁿ-th iterations
             (if (zero? (unsafe-fxand n n₀))
                 (exec/bump-loop-count-and-check rec n₀ n)
                 (exec/bump-loop-count rec n₀ n)))
           ;; `f` loops back first time
           (exec/mark-first-loop)))]
    ;; `f` is seen first time in this call chain
    [else (printf "applying ~a to ~a: unseen~n" f xs) (exec/mark-seen)]))

(: apply/guard : (∀ (X Y) (X * → Y) X * → Y))
;; FIXME: Shameless duplicate of `apply/guard/restore`
(define (apply/guard f . xs)
  (define (exec) (apply f xs))

  (define (exec/mark-seen)
    (dynamic-wind
      (λ () (hash-set! call-stack f #t))
      exec
      ;; Always clean up after loop to avoid excessive leaks
      (λ () (hash-remove! call-stack f))))

  (define (exec/mark-first-loop)
    (hash-set! call-stack f ((inst mcons Positive-Integer Record) 1 (Record xs (list (init-sc-graph (length xs))))))
    (exec))

  (: exec/bump-loop-count : (MPairof Positive-Integer Record) Positive-Integer → Y)
  (define (exec/bump-loop-count rec n)
    (set-mcar! rec n)
    (exec))

  (: exec/bump-loop-count-and-check : (MPairof Positive-Integer Record) Positive-Integer → Y)
  (define (exec/bump-loop-count-and-check rec n)
    (define r (mcdr rec))
    (define xs₀ (Record-last-examined-args r))
    (define Gs₀ (Record-last-sc-graph r))
    (dynamic-wind
      (λ ()
        (define Gs (update-record r f xs))
        (set-mcar! rec n)
        (set-Record-last-examined-args! r xs)
        (set-Record-last-sc-graph! r Gs))
      exec
      ;; Clean up after to avoid excessive leaks
      (λ ()
        (set-mcar! rec (assert (sub1 n) positive?))
        (set-Record-last-examined-args! r xs₀)
        (set-Record-last-sc-graph! r Gs₀))))

  (cond
    ;; `f` detected as loop entry
    [(hash-ref call-stack f #f)
     =>
     (λ (rec)
       (if (mpair? rec)
           ;; `f` loops back second time onwards
           (let* ([n₀ (mcar rec)]
                  [n (add1 n₀)])
             ;; bump loop count, check against SCT violation at 2ⁿ-th iterations
             (if (zero? (unsafe-fxand n n₀))
                 (exec/bump-loop-count-and-check rec n)
                 (exec/bump-loop-count rec n)))
           ;; `f` loops back first time
           (exec/mark-first-loop)))]
    ;; `f` is seen first time in this call chain
    [else (exec/mark-seen)]))

(: update-record : Record Procedure (Listof Any) → (Pairof SC-Graph (Listof SC-Graph)))
(define (update-record r f xs)
  (match-define (Record xs₀ Gs₀) r)
  (define G (make-sc-graph xs₀ xs))
  (match (find-sc-violation G Gs₀)
    [(? values G-err) (err G-err f xs₀ xs)]
    [#f (cons G Gs₀)]))

(: err : SC-Graph Procedure (Listof Any) (Listof Any) → Nothing)
(define (err G f xs₀ xs)
  (define (graph->lines [G : SC-Graph])
    (for/list : (Listof String) ([(edge ↝) (in-hash G)])
      (format "  * ~a ~a ~a" (car edge) ↝ (cdr edge))))
  (define (args->lines [xs : (Listof Any)])
    (for/list : (Listof String) ([(x i) (in-indexed xs)])
      (format "  * arg ~a: ~a" i x)))
  (define lines
    `(,(format "Recursive call to `~a` has no obvious descent on any argument" f)
      "- Preceding call:" ,@(args->lines xs₀)
      "- Subsequent call:" ,@(args->lines xs)
      "Size-change violating graph:" ,@(graph->lines G)))
  (error 'possible-non-termination (string-join lines "\n")))