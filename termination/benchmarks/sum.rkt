#lang racket/base

(require "../main.rkt")

(define (sum n)
  (if (zero? n) 0 (+ n (sum (sub1 n)))))

;; expands to:
#;(define-values
 (sum)
 (lambda (n)
   (if (#%app zero? n)
     '0
     (#%app
      +
      n
      (let-values (((f) sum) ((temp2) (#%app sub1 n)))
        (if (#%app terminating-function? f)
          (let-values ()
            (#%app apply/termination¹ (#%app unsafe-struct-ref f '0) temp2))
          (if (#%app divergence-ok?)
            (let-values () (#%app f temp2))
            (let-values () (#%app apply/termination¹ f temp2)))))))))

(collect-garbage) (collect-garbage) (collect-garbage)
(time (begin/termination (void (sum 100000000))))
