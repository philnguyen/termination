#lang info

(define collection "termination")

(define deps '("profile-lib"
               "r5rs-lib"
               "rackunit-lib"
               "base"
               "typed-racket-lib"
               "typed-racket-more"))

(define pkg-desc "Modified Racket that supports dynamic enforcement of termination")

(define pkg-authors '(pcn))
