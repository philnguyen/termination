#lang typed/racket/base

(require racket/match
         racket/set
         typed/racket/unsafe
         "flattened-parameter.rkt")

(provide SC-Graph
         make-sc-graph
         concat-graph
         find-sc-violation)
(unsafe-provide with-<?)

;; A size-change graph tracks how a function calls itself,
;; where each edge denotes a "must" non-ascendence between argument indices
(define-type SC-Graph (Immutable-HashTable (Pairof Integer Integer) Dec))

;; Transition between values based on some well-founded partial order
;; - `↓` is definite descendence
;; - '↧` is definite non-ascendence
;; - `#f` is conservative "don't know"
(define-type Dec (U '↓ '↧))

(: concat-graph : SC-Graph SC-Graph → SC-Graph)
(define (concat-graph G₁ G₂)
  (for*/fold ([G* : SC-Graph (hash)])
             ([(edge₁ ↝₁) (in-hash G₁)]
              [i (in-value (cdr edge₁))]
              [(edge₂ ↝₂) (in-hash G₂)]
              #:when (eq? i (car edge₂)))
    (hash-update G* (cons (car edge₁) (cdr edge₂))
                 (λ ([↝₀ : Dec]) (Dec-best ↝₀ ↝₁ ↝₂))
                 (λ () '↧))))

(: find-sc-violation : SC-Graph (Listof SC-Graph) → (Option SC-Graph))
(define (find-sc-violation G Gs)
  (define (violating? [G : SC-Graph])
    (and (equal? G (concat-graph G G))
         (not (for/or : Boolean ([(edge dec) (in-hash G)])
                (and (eq? dec '↓)
                     (eq? (car edge) (cdr edge)))))))
  (cond
    [(violating? G) G]
    [(null? Gs) #f]
    [else (find-sc-violation (concat-graph (car Gs) G) (cdr Gs))]))

(: make-sc-graph : (Listof Any) (Listof Any) → SC-Graph)
;; Make size-change graph from comparing old and new argument lists
(define (make-sc-graph xs₀ xs₁)
  (define cmp (let ([≺ (<?)]) (λ (x y) (if (equal? x y) '↧ (and (≺ y x) '↓)))))
  (for*/hash : SC-Graph ([(v₀ i₀) (in-indexed xs₀)]
                         [(v₁ i₁) (in-indexed xs₁)]
                         [?↓ (in-value (cmp v₀ v₁))] #:when ?↓)
    (values (cons i₀ i₁) ?↓)))

(define Dec-best : (Dec * → Dec)
  (match-lambda*
   [(list '↧ ...) '↧]
   [_ '↓]))

;; FIXME hack
(require typed/racket/unsafe)
(unsafe-require/typed racket/base
                      [mcar (MPairTop → Any)]
                      [mcdr (MPairTop → Any)])
(: <?:default : Any Any → Boolean)
;; Simple default implementation of well-founded strict partial order on data
(define (<?:default x y)
  (define (≤? x y) (or (equal? x y) (<?:default x y)))
  
  (cond [(integer? y) (and (integer? x) (< -1 x y))]
        [(pair? y) (or (≤? x (car y))
                       (≤? x (cdr y))
                       #;(and (pair? x)
                            (or (<?:default (car x) (car y))
                                (<?:default (cdr x) (cdr y)))))]
        [(mpair? y) (or (≤? x (mcar y))
                        (≤? x (mcdr y))
                        #;(and (mpair? x)
                             (or (<?:default (mcar x) (mcar y))
                                 (<?:default (mcdr x) (mcdr y)))))]
        [else #f]))

(define-parameter <? : (Any Any → Boolean) <?:default)
