#lang typed/racket/base

(require set-extras
         typed/racket/unsafe
         "lang.rkt"
         "analyze.rkt")

(unsafe-provide do-analyze) ; no contract for Syntax

(unsafe-require/typed "parse.rkt"
  [parse-module (Syntax → M)])

(: do-analyze : Syntax → (Values S (℘ L)))
(define (do-analyze stx) (analyze (parse-module stx)))
