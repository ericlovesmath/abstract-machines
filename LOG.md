## Log

# Week 1

* Research into SECD Machine
* Set up OCaml environment for SECD
* Implement simple parts of SECD Machine (Essentially an RPN calculator)
* Write S-exp module to write SECD instructions

# Week 2

* Write (Lazy) Parser Combinator library (mimicking Haskell's `Parsec`)
* Restructure project to add tests
* Remove S-exp module for `Intro.parse` using parser combinators
* Convert AST to SECD instruction through `Flatten`
* Implement functional lists in SECD machine
* Implement minimal REPL

# Week 3

* Implement Lambda functions, if statements, and more primitive functions
* Add syntactic sugar for `let` bindings to add functions (OCaml style)
* Add `Assign` to allocate variables to correct environment locations
* Add more tests (like Y-Combinator Factorial)

# Week 4

* Update SECD REPL to support loading files and multiline input
* Set up README's and LOG
* Adding Documentation
* Give up on SECD letrec (partial implementation in branch)
* Adding `.src` files for testing

# Week 5

* CEK Machine Lambda Calculus interpreter
* Fix `dune test`

# Week 6

* CPS style ANF conversion
* CEK Minimal front end to compilation pipeline working
* Native `letrec` for CEK Machine
* Support for first-class primitives avoiding slow abstractions
* Add debug printing script for CEK

# Week 7

* Refactor testing error messages, adding tests that expect failure
* Krivine Minimal Implementation
* Sanity testing Krivine machine is lazy and handles infinite lists
* Write Krivine Frontend
* Write tests for infinite lists!
* Recursion (with Y-combinator, but tail recursive)
* Cleaning up Krivine codebase

# Week 8

* Rewrite Krivine to follow original paper more
* Refactoring and Final Report

# Week 11 (Following Term!)

* Refactor Krivine to by Lazy on Cons (on new version)
* Cleaning up Krivine codebase
* Merge CEK and Krivine codebases together with shiny functors

    - Shiny testing per compiler
    - Debug passes using `ppx_sexp_conv` in CEK and Krivine
    - Merge SECD code as well (testing and debug)
    - Rename and move everything to one folder!

* Native booleans
* Desugaring list, and, or, not, neq
* Add `base` option with small standard lib

# Week 12

* Rewrite parser combinators to use monadic form instead
* Add comment parser
* Add desugaring for `let*` and `letrec*`, add tests
* Use `(include_subdirs qualified)` (this took too long)
* Read Turner Paper on SK Machine and Graph Reduction
* Implement mini GADT version of SK Machine

# Week 13

* Rudimentary SK-Machine (not using graph reduction) made
* Full (poorly written) Frontend -> SK pipeline written
* Fix SECD `letrec`
* Fix and add more tests

# Week 14

* Record failure and continue tests
* Rename SKI to SK
* Web frontend to REPL using Bonsai
* Add Nix and Makefile
* Test graph reduction SK machine with bunch of tests
* Implement SK machine into pipeline, most tests work (aside from recursion)

    - Note recursion is failing because of laziness issues, not with `Y`
    - Replaced `Y` with `SSK(S(K(SS(S(SSK))))K)` while investigating

* Trying to work on visualization in Python
* Add optimize pass (incomplete, but as described in Turner's paper)

# Week 15

* Complete optimize pass
* True WHNF reduction, removing hacky `normalize` function

    - Y combinator and C combinator works as expected!

* Clean up playground files
* Add visualization of graphs in SK machine using dot files

    - This may need cleaning up, requires calling `log_graph` manually
    - Option to show unreachable nodes or not

* Interface `.mli` files

# Week 16

* Fix `letrec` SECD Issue, I'm finally free.
* Laziness of Cons
* Fix tests for everything because everything WORKS and the world is REVOLVING FINALLY
* Update README to use Makefile
* Minimal CESK Implementation

# Week 17

* Fully integrate CESK Implementation
* Add support for imperative features like `begin`, `set!`, and `while`
* Deal with ANF conversion issues
* Implement `call/cc` with example tests
* Add a bunch of imperative and `call/cc` tests
* Add `#u` to all machines (now more relevant due to `set!`)

## Working Notes

* Improve Graph Reduction code
* Add support for bindings of infinite lists
* Add ability to inject AST forms into arbitrary passes
* Generators, catching errors with `call/cc`
* Shorthand `if` and `cond` form syntax
* Implement `display` and `read`
* Read up on CESK and WAM

## Todo

* Add examples to README
* Mutual Recursion
* Add top level defines (environment/machine passing)

    * Actual Base implementation with real REPL

* Consider CESK Machine, Forth, P-System, Smalltalk-80, etc.
* Typechecking and Error handling

- Stupid casting
- Laziness with single values bound to them
