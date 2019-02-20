#lang racket/base

(require termination)

(define (f x)
  (cond [(or (not (integer? x)) (= x 0)) 0]
        [(< x 0) (f (+ x 1))]
        [else (f (- x 1))]))
(define (dec n)
  (cond [(or (not (integer? n)) (<= n 0)) 255]
        [else (- n 1)]))
(define (foo i j)
  (if (= i 1)
      (if (= j 1) 0 (foo (dec j) (dec j)))
      (foo (dec i) j)))

(begin/termination (f -10))
(begin/termination (f 10))
(begin/termination (foo 4 5))
;; `foo` on negatives need custom ordering
(with-custom-< (λ (x y)
                 (define (normalize x) (if (>= x 0) x (- 255 x)))
                 (< (normalize x) (normalize y)))
  (begin/termination (foo -3 -2)))
