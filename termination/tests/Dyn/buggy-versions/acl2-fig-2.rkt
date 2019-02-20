#lang racket/base

(require termination)

(define (f x)
  (cond [(or (not (integer? x)) (= x 0)) 0]
        [(< x 0) (f (- x 1))]
        [else (f (+ x 1))]))
(define (dec n)
  (cond [(or (not (integer? n)) (<= n 0)) 255]
        [else n]))
(define (foo i j)
  (if (= i 1)
      (if (= j 1) 0 (foo (dec j) j))
      (foo (dec i) j)))

(require rackunit)
(check-exn exn? (λ () (begin/termination (f -10))))
(check-exn exn? (λ () (begin/termination (f 10))))
(check-exn exn? (λ () (begin/termination (foo 4 5))))
;; `foo` on negatives need custom ordering
(check-exn exn? (λ () (with-custom-< (λ (x y)
                                       (define (normalize x) (if (>= x 0) x (- 255 x)))
                                       (< (normalize x) (normalize y)))
                        (begin/termination (foo -3 -2)))))
