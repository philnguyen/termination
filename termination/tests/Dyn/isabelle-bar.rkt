#lang racket/base

(require racket/match
         termination)

(define bar
  (terminating-function/c 
   (match-lambda**
    [(0               (? positive? n) m) (bar m m m)]
    [((? positive? v) n               m) 0]
    [(k               0               m) 0])))
(bar 3 4 5)
