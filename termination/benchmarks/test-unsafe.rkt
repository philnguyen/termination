#lang s-exp "../unsafe.rkt"

(define/termination (sum n)
  (if (zero? n) 0 (+ n (sum (sub1 n)))))
(time (void (sum 10000000)))


;(begin/termination 42)
;;
;; expands to: ((terminating-function (λ () 42)))
;;
;; error message:
;; termination/termination/unsafe.rkt:174:4: terminating-function: cannot use identifier tainted by macro transformation
;   in: terminating-function

