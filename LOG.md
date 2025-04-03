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




# Week 9

* Refactor Krivine to by Lazy on Cons (on new version)
* Cleaning up Krivine codebase
* Merge CEK and Krivine codebases together with shiny functors

## TODO

* GADT implementation for ANF conversion
* Add examples to README
* Fix bad code and ADT names
* Add comments
* Mutual Recursion
* Add top level defines
* Fix `letrec` for SECD
* Integrate different compilers as modules together to one file
* Cleaning up Krivine codebase (renaming needed)
* Use `(include_subdirs qualified)`
* Fix `dune test` to test each backend

* Refactor everything
* Consider CESK Machine, Forth, P-System, Smalltalk-80, etc.

* Add more syntactic sugar
* Add comment parser
* Typechecking and Error handling
* Generalize tests for different machines
