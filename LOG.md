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

## TODO

* Add top level defines
* Work on CEK implementation

## Long Term TODO

* Add more syntactic sugar
* Typechecking and Error handling
* Generalize tests for different machines
