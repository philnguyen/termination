#lang racket/base

(define (sort-list obj pred)

  (define (loop l)
    (if (and (pair? l) (pair? (cdr l)))
        (split l '() '())
        l))

  (define (split l one two)
    (if (pair? l)
        (split (cdr l) two (cons (car l) one))
        (merge (loop one) (loop two))))

  (define (merge one two)
    (cond ((null? one) two)
          ((pred (car two) (car one))
           (cons (car two)
                 (merge (cdr two) one)))
          (else
           (cons (car one)
                 (merge (cdr one) two)))))

  (loop obj))

(require "../../../../unsafe.rkt" "../common.rkt")
(run-bm [N 10000]
        (let ([l (map symbol->string (build-list N (λ _ (gensym))))])
          (with-custom-< ≺
            (begin/termination (sort-list l string<=?)))))
