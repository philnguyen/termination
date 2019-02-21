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

The image has been tested to work with Virtualbox 6.0.
Instructions for
[downloading and installing Virtualbox](https://www.virtualbox.org/wiki/Downloads)
can be found on the official site.

1. Download the [OVA image](TODO link)
(if you don't have a Dropbox account, there is **no need to sign up**,
just dismiss the sign-up dialog.)

2. Launch the image: on most Linux or Windows desktops, double-clicking the file will do.
Otherwise from Virtualbox, choose `File -> Import Appliance`. It is reccommended
that you give the image at least `2048MB` of memory.

3. The image runs Lubuntu 18.10 64bit that should log in automatically.
   If for any reason it requires logging in, username and password are both `reviewer`.

4. After the desktop loads, launch the terminal using the icon on the desktop.

5. To automate *all* steps described in the next sections, execute:

        make
        
An example of the expected output can be found at
[log.txt](https://github.com/philnguyen/termination/tree/pldi-19-ae/log.txt).

Below, we describe each step, along with the specific `make` target to just run
that step.
We assume the working directory is `/home/reviewer/`

### Testing the dynamic termination checker

#### Running the tests

##### Testing our tools

The test suite contains many programs collected from other work.

* To test the dynamic checker on the tests in `Table 1`, run `make test-dynamic`.
  Outputs are results of individual programs.
        
* To generate the benchmark results in `Figure 10`, run `make benchmark-dynamic`.
  If the machine has less that 4G of memory, some of the benchmarks may be aborted in the middle.

##### Checking results from other tools

* To try Liquid Haskell, launch the browser with the default page set to
  [Liquid Haskell](http://goto.ucsd.edu:8090/index.html#?demo=blank.hs),
  then try tests in [tests/LH/](https://github.com/philnguyen/termination/tree/pldi-19-ae/termination/tests/LH).
  Below are permalinks created for each test:
  
  + [sct-1](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550704801_16372.hs),
    [sct-2](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550704940_16374.hs),
    [sct-3](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550704966_16377.hs),
    [sct-4](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550704989_16379.hs),
    [sct-5](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705009_16381.hs),
    [sct-6](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705036_16383.hs)
  + ~~ho-sct-ack~~,
    [ho-sct-fg](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705083_16385.hs),
    [ho-sct-fold](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705103_16387.hs)
  + [isabelle-perm](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705121_16389.hs),
    [isabelle-f](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705155_16392.hs),
    [isabelle-foo](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705177_16394.hs),
    [isabelle-bar](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705202_16397.hs),
    [isabelle-poly](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705219_16399.hs)
  + [acl2-fig-2](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550704551_16364.hs),
    [acl2-fig-6](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550704641_16366.hs),
    [acl2-fig-7](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550704719_16370.hs)
  + [lh-gcd](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705241_16401.hs),
    [lh-map](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705259_16403.hs),
    [lh-merge](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705279_16405.hs),
    [lh-range](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705334_16409.hs),
    [lh-tfact](http://goto.ucsd.edu:8090/index.html#?demo=permalink%2F1550705307_16407.hs)

* To try Isabbelle, double-click the `Isabelle.sh` icon on the Desktop, which should by default open tests in
  [tests/Isabelle/](https://github.com/philnguyen/termination/tree/pldi-19-ae/termination/tests/Isabelle).
  Within Isabelle, double click on any test to view and start checking it.

* To try ACL2, launch the `ACL2` icon on the Desktop, which should by default open tests in
  [tests/ACL2/](https://github.com/philnguyen/termination/tree/pldi-19-ae/termination/tests/ACL2).
  The `.lisp` files contain the source, and the `.lisp.a2s` files contain checking result.
  
    
#### Trying your own programs

Directory [tests/Dyn](https://github.com/philnguyen/termination/tree/pldi-19-ae/termination/tests/Dyn)
contains many example of that use termination contracts.

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

Racket programs can either be run from DrRacket or from the command line as `racket file-name.rkt`.

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
