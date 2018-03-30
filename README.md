[![Build Status](https://travis-ci.org/philnguyen/termination.svg?branch=master)](https://travis-ci.org/philnguyen/termination) termination
=========================================

Racket with modified `#%app` for dynamic enforcement of (size-change) termination.

### Install

```
raco pkg install termination
```

### Usage

* `(terminating-function/c e)`: ensures `e` computes a function that run finitely on any argument
* `(define/termination (f x ...) e ...)`: shorthand for `(define f (terminating-function/c (λ (x ...) e ...)))

### Examples

```racket
#lang racket/base
(require termination)

(define/termination (fact n)
  (if (zero? n) 1 (* n (fact (sub1 n)))))

(fact 10) ; ==> ok
(fact -1) ; ==> error
```

More examples are in [test.rkt](https://github.com/philnguyen/termination/blob/master/termination/test.rkt).

