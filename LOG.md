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
* TODO SECD

## TODO

* SECD Debug and Fix
* Refactor everything
* Read Krivine

* Fix bad code and ADT names
* Add comments (lambda-sigma calculus)
* Add more tests (like nested letrec or mutual recursion)

* Add top level defines
* Fix `letrec` for SECD
* Integrate CEK and SECD as modules together to one file

## Long Term TODO

* Add more syntactic sugar
* Add comment parser
* Typechecking and Error handling
* Generalize tests for different machines
