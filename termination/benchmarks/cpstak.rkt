#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         cpstak.sch
; Description:  continuation-passing version of TAK
; Author:       Will Clinger
; Created:      20-Aug-87
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))
  (tak x y z (lambda (a) a)))

#;(time (cpstak 18 12 6))

(begin
  (require "../main.rkt")
  (time (begin/termination (cpstak 18 12 6))))

#| Can't do automatically in Lean either
def filter (p: nat -> bool): list nat → list nat
| []       := []
| (a :: l) :=
  match p a with
  |  tt := a :: filter l
  |  ff := filter l
  end

def lt: nat -> nat -> bool
| (m + 1) (n + 1) := lt m n
| 0 _ := ff
| _ _ := tt

def tak : nat -> nat -> nat -> (nat -> nat) -> nat
| (x+1) (y+1) (z+1) k :=
  match lt y x with
  | tt := tak x (y+1) (z+1) (fun v1, tak y (z+1) (x+1) (fun v2, tak z (x+1) (y+1) (fun v3, tak v1 v2 v3 k)))
  | ff := k (z+1)
  end
| _ _ _ _ := 42
|#
