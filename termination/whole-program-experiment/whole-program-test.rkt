#lang racket/base

(require racket/list
         racket/unsafe/ops
         profile
         "size-change-graph-whole-program.rkt")

(struct terminating-function (unwrapped)
  #:transparent
  #:property prop:procedure
  (λ (f . args)
    (define g (unsafe-struct-ref f 0))
    (define-values (xs st) (split-at args (sub1 (length args))))
    (apply g (append xs (list (apply update-stack (car st) g xs))))))

(define (terminating-function/c f)
  (if (or (primitive? f) (terminating-function? f)) f (terminating-function f)))

(define (fact n st)
  (if (zero? n) 1 (* n (let ([n* (sub1 n)])
                         (fact n* (if (divergence-ok? st)
                                      st
                                      (update-stack st fact n*)))))))

(define (a m n st)
  (cond [(zero? m) (+ 1 n)]
        [(zero? n) (let ([m* (- m 1)]
                         [n* 1])
                     (a m* n* (if (divergence-ok? st)
                                  st
                                  (update-stack st a m* n*))))]
        [else (let ([m* (- m 1)]
                    [n* (let ([m* m]
                              [n* (- n 1)])
                          (a m* n* (if (divergence-ok? st)
                                       st
                                       (update-stack st a m* n*))))])
                (a m* n* (if (divergence-ok? st)
                             st
                             (update-stack st a m* n*))))]))

(define (ack m n)
  (cond [(zero? m) (+ 1 n)]
        [(zero? n) (ack (- m 1) 1)]
        [else (ack (- m 1) (ack m (- n 1)))]))

(define (rev ls st) (r1 ls '() st))
(define (r1 ls a st)
  (if (null? ls)
      a
      (let ([ls* (cdr ls)]
            [a* (cons (car ls) a)])
        (r1 ls* a* (if (divergence-ok? st)
                       st
                       (update-stack st r1 ls* a*))))))

(define-syntax-rule (gc)
  (begin (collect-garbage) (collect-garbage) (collect-garbage)))

(define l (range 10000))

(begin
  (gc) (time (ack 3 10))
  (gc) (time ((terminating-function/c a) 3 10 mt-call-stack))
  ;(gc) (time (void ((terminating-function/c a) 3 10 mt-call-stack)))
  ;(gc) (time (void ((terminating-function/c fact) 10000 mt-call-stack)))
  ;(gc) (time (void ((terminating-function/c rev) l mt-call-stack)))
  )
