[![Build Status](https://travis-ci.org/philnguyen/termination.svg?branch=pldi-19-ae)](https://travis-ci.org/philnguyen/termination)
Artifact for "Size-change Termination as a Contract"
=========================================

This repository contains the artifact for the research done in the paper
[*Size-change Termination as a Contract*]
(https://github.com/philnguyen/termination/blob/pldi-19-ae/paper/main.pdf).

There are three components to this artifact:
* The dynamic termination checker, implemented as a language extension for Racket
* The static checker, implemented as a stand-alone command-line tool
* The set of tests used in the paper

There are two options to evaluate the artifact:
* Option 1: by [obtaining the self-contained Virtualbox image](#option-1-obtain-the-self-contained-virtualbox-image)
* Option 2: by [cloning and building the repositories](#option-2-build-packages-from-source)

## Option 1: Obtain the self-contained Virtualbox image

The more convenient method for testing the artifact is to download and run a
virtual machine that contains the dynamic checker, static checker, tests, and all
their dependencies.

The image has been tested to work with Virtualbox `5.1.26`.
Instructions for
[downloading and installing Virtualbox](https://www.virtualbox.org/wiki/Downloads)
can be found on the official site.

1. Download the [OVA image](TODO link)
(if you don't have a Dropbox account, **no need to sign up**,
just dismiss the sign-up dialog.)

2. Launch the image: on most Linux or Windows desktops, double-clicking the file will do.
Otherwise from Virtualbox, choose `File -> Import Appliance`. It is reccommended
that you give the image at least `2048MB` of memory.

3. The image runs Lubuntu 16.04 64bit with log-in information:

  * Username: `reviewer`
  * Password: `reviewer`

4. After the desktop loads, launch the terminal using the icon on the desktop.

5. To automate *all* steps described in the next sections, execute:

        cd artifact
        make
        
An example of the expected output can be found at
[log.txt](https://github.com/philnguyen/termination/tree/pldi-19-ae/log.txt).

Below, we describe each step, along with the specific `make` target to just run
that step.
We assume the working directory is `/home/reviewer/artifact`

### Testing the dynamic termination checker

#### Running the tests

##### Testing our tools

The test suite contains many programs collected from other work.

* To test the dynamic checker on the tests in `Table 1`, run:

        make test-dynamic 
        
* To generate the benchmark results in `Figure 10`, run:

        make benchmark-dynamic

##### Checking results from other tools

* To try Liquid Haskell, launch the browser with the default page set to
  [Liquid Haskell](http://goto.ucsd.edu:8090/index.html#?demo=blank.hs),
  then try tests in [tests/LH/](TODO link)

* To try Isabbelle, launch the Isabelle icon on the Desktop, which should by default open tests in
  [tests/Isabelle/](TODO link).

* To try ACL2, launch the Eclipse icon on the Desktop, which should by default open tests in
  [tests/ACL2/](TODO link).
  
    
#### Trying your own programs

Directories [tests/Dyn](TODO link) and [tests/Dyn/buggy-versions](TODO link)
contain many example of terminating and non-terminating programs, respectively,
that use termination contracts.

To try the dynamic checker on your own Racket program,
add `(require termination)` to the file. Then:

* For each function `f` to be enforced to terminate when run, wrap it around
  `terminating-function/c` as in `(terminating-function/c f)`.
  
* For each definition of function `f` whose termination is to be enforced when run,
  use `define/termination` instead of `define`.
  
        (define/termination (f x ...) e ...)
        
  is syntactic sugar for
  
        (define f (terminating-function/c (λ (x ...) e ...)))
  
* For each expression `e` whose termination is to be enforced, wrap it in
  `begin/termination` as in `(begin/termination e)`.
  
        (begin/termination e)
        
  is syntactic sugar for
  
        ((terminating-function/c (λ () e)))
        
* To use a custom partial-order `ord` during the evaluation of `e`,
  wrap it with `with-custom-<` as in `(with-custom-< ord e)`.

Below is one self-contained example:

```racket
#lang racket/base
(require termination)

(define/termination (fact n)
  (if (zero? n) 1 (* n (fact (sub1 n)))))

(fact 10) ; ==> ok
(fact -1) ; ==> error
```


### Testing the static termination checker

#### Runing the tests

To run the static checker on tests in `Table 1`, run:

    make test-static

#### Trying your own programs 

Directories [tests/](TODO link) and [tests/buggy-versions](TODO link)
contain many examples of terminating and non-terminating programs, respectively.

For historical reasons, the dynamic and static systems were developed independently,
and the syntax used in both systems are not identical.
To try the static checker on your own Racket program,
add `(require soft-contract/fake-contract)` to the file.
When exporting a function `f` with the function contract `->`,
`#:total? #t` indicates a promise that `f` terminates.

Below is one self-contained example:

```racket
#lang racket/base
(require soft-contract/fake-contract)

(define (fact n)
  (if (zero? n) 1 (* n (fact (sub1 n)))))

(provide/contract
  [f (exact-nonnegative-integer? . -> . exact-nonnegative-integer? #:total? #t)])
```

## Option 2: Build packages from source

Before building the checkers,
download [Racket 7.2](https://download.racket-lang.org/).
The projects might not build with earlier releases.

### Building and installing the dynamic checker

    git clone [TODO]
    cd termination/termination
    git checkout pldi-19-ae
    raco pkg install --deps search-auto

The package `termination` is then available to be required in any Racket module.
Remaining tests are similar to Option 1.

### Building and installing the static checker

    git clone [TODO]
    cd soft-contract/soft-contract
    git checkout pldi-19-ae
    raco pkg install --deps search-auto

The command `raco scv` is then available to be used.
Remaining tests are similar to Option 1.
