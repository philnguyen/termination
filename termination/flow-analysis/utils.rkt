#lang typed/racket/base

(provide cycles)

(require racket/set
         set-extras)

(: cycles (∀ (X) (HashTable X (℘ X)) X → (℘ X)))
;; Find all cycles in graph starting at given node
(define (cycles graph src)
  (define visited : (Mutable-HashTable X #t) (make-hash))
  (define cycles : (℘ X) ∅)
  (let visit ([node : X src] [seen : (℘ X) ∅])
    (cond
      [(∋ seen node) (set! cycles (set-add cycles node))]
      [(hash-has-key? visited node) (void)]
      [else
       (hash-set! visited node #t)
       (define seen* (set-add seen node))
       (for ([tgt (in-set (hash-ref graph node mk-∅))])
         (visit tgt seen*))]))
  cycles)
