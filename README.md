# Abstract Machine Exploration

Implementing Abstract machines targeted by functional compilers:

- **SECD Machine**: Described in [The Mechanical Evaluation of Expressions](https://jhc.sjtu.edu.cn/~yutingwang/files/fp/landin-1964.pdf) by P. J. Ladin, the SECD Machine is designed to evaluate the lambda calculus. It uses a Stack (S) to store intermediary values, an Environment (E) to store mappings from variables to values, a Control (C) to identify the code currently being executed, and a Dump (D) to store previous contexts of the machine to jump back later.

- **CEK Machine**: TODO

Instructions (execute inside folder for any `<machine>`)

- REPL: `dune exec <machine>` (Suggested to run with [rlwrap](https://github.com/hanslub42/rlwrap) for convenience)
- Interactive Shell: `dune utop`, then `open Lib;;`
- Tests: `dune test`
