[![Build Status](https://travis-ci.org/philnguyen/termination.svg?branch=master)](https://travis-ci.org/philnguyen/termination) termination
=========================================

Racket with modified `#%app` for dynamic enforcement of (size-change) termination.

### Install

```
raco pkg install termination
```

### Usage
```racket
#lang racket/base
(require termination)
```

* `(terminating-function/c e)`: ensures `e` computes a function that run finitely on any argument
* `(define/termination (f x ...) e ...)`: shorthand for `(define f (terminating-function/c (λ (x ...) e ...)))

### Examples

Examples are in [test.rkt](https://github.com/philnguyen/termination/blob/master/termination/test.rkt).

