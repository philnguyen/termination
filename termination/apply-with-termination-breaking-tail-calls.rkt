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
         "size-change-graph.rkt")

(struct Record ([last-examined-args : (Listof Any)]
                [last-sc-graph : SC-Graph])
  #:mutable
  #:transparent)

(define call-stack ((inst make-hasheq Procedure (U #t (MPairof Positive-Integer Record)))))

(define (divergence-ok?) (hash-empty? call-stack))

(: apply/termination (∀ (X Y) (X * → Y) X * → Y))
(define (apply/termination f . xs)
  #;(begin
    (printf "apply/termination ~a with:~n" f)
    (for ([(f r) (in-hash call-stack)])
      (printf "- ~a~n" f)
      (printf "  -> ~a~n" r))
    (printf "~n"))
  (define (exec) (apply f xs))

  (define (exec/record)
    (dynamic-wind
      (λ () (hash-set! call-stack f #t))
      exec
      (λ () (hash-remove! call-stack f))))

  (define (exec/mark-first-loop)
    (dynamic-wind
      (λ () (hash-set! call-stack f ((inst mcons Positive-Integer Record) 1 (Record xs (init-sc-graph (length xs))))))
      exec
      (λ () (hash-set! call-stack f #t))))

  (: exec/bump-count : (MPairof Positive-Integer Record) Positive-Integer Positive-Integer → Y)
  (define (exec/bump-count rec n₀ n)
    (dynamic-wind
      (λ () (set-mcar! rec n))
      exec
      (λ () (set-mcar! rec n₀))))

  (: exec/check : (MPairof Positive-Integer Record) Positive-Integer Positive-Integer → Y)
  (define (exec/check rec n₀ n)
    (define r (mcdr rec))
    (define xs₀ (Record-last-examined-args r))
    (define G₀ (Record-last-sc-graph r))
    (dynamic-wind
      (λ ()
        (define G (update-record r f xs))
        (set-mcar! rec n)
        (set-Record-last-examined-args! r xs)
        (set-Record-last-sc-graph! r G))
      exec
      (λ ()
        (set-mcar! rec n₀)
        (set-Record-last-examined-args! r xs₀)
        (set-Record-last-sc-graph! r G₀))))
  
  (cond
    ;; `f` detect as loop entry
    [(hash-ref call-stack f #f)
     =>
     (λ (rec)
       (if (mpair? rec)
           ;; `f` loops back second time onwards
           (let* ([n₀ (mcar rec)]
                  [n (add1 n₀)])
             ;; bump loop count, check against SCT violation at 2ⁿ-th iterations
             (if (zero? (unsafe-fxand n n₀))
                 (exec/check rec n₀ n)
                 (exec/bump-count rec n₀ n)))
           ;; `f` loops back first time
           (exec/mark-first-loop)))]
    ;; `f` is seen first time in this call chain
    [else (exec/record)]))

(: update-record : Record Procedure (Listof Any) → SC-Graph)
(define (update-record r f xs)
  (match-define (Record xs₀ G₀) r)
  (define G (make-sc-graph xs₀ xs))
  (or (and (strictly-descending? G)
           (let ([G* (concat-graph G₀ G)])
             (and (strictly-descending? G*)
                  G*)))
      (err G₀ G f xs₀ xs)))

(: err : SC-Graph SC-Graph Procedure (Listof Any) (Listof Any) → Nothing)
(define (err G₀ G f xs₀ xs)
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
      "Initial graph:" ,@(graph->lines G₀)
      "Step graph:" ,@(graph->lines G)))
  (error 'possible-non-termination (string-join lines "\n")))
