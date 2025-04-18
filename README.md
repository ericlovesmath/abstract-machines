# Abstract Machine Exploration

Implementing Abstract machines targeted by functional compilers:

- **SECD Machine**: Described in [The Mechanical Evaluation of Expressions](https://jhc.sjtu.edu.cn/~yutingwang/files/fp/landin-1964.pdf) by P. J. Ladin, the SECD Machine is designed to evaluate the lambda calculus. It uses a Stack (S) to store intermediary values, an Environment (E) to store mappings from variables to values, a Control (C) to identify the code currently being executed, and a Dump (D) to store previous contexts of the machine to jump back later.

- **CEK Machine**: A modification of the SECD Machine, but stateful operations (storage in stack / dump) are stored in Continuations (K) instead, along wth the same Control (C) and Environments (E). Implementation does not use De Brujin indicies, and is higher level than SECD. Written as a continuation passing style compiler, with native tail recursion and more efficient use of memory than the SECD Machine.

- **Krivine Machine**: Machine most similar to direct beta and eta reduction of the lambda calculus, reducing code to lambdas and repeatedly performing weak head normal form reductions. Call-by-Name by nature, this implementation is lazily evaluated.

Instructions (execute inside folder for any `<machine>`)

- REPL: `dune exec abstract_machines -- -machine <machine>` (Suggested to run with [rlwrap](https://github.com/hanslub42/rlwrap) for convenience)
- Debug: `dune exec abstract_machines -- -machine <machine> -debug`
- Tests: `dune test` (need to `dune clean && dune build` when modifying `tests/*.src` files)

See `test/tests/*.src` files for examples!
