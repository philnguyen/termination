#lang racket/base

(provide (struct-out terminating-function)
         terminating-function/c)

(struct terminating-function (unwrapped) #:transparent
  #:property prop:procedure 0)

(define (terminating-function/c f)
  (if (or (primitive? f) (terminating-function? f)) f (terminating-function f)))
