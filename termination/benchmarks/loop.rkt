#lang racket/base

(require "../main.rkt")

(define (loop) (loop))

(time
 (with-handlers ([exn? (λ _ 'errored)])
   (begin/termination (loop))))
