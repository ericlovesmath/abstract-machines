# Abstract Machine Exploration

Implementing Abstract machines targeted by functional compilers:

- **SECD Machine**: Described in [The Mechanical Evaluation of Expressions](https://jhc.sjtu.edu.cn/~yutingwang/files/fp/landin-1964.pdf) by P. J. Ladin, the SECD Machine is designed to evaluate the lambda calculus. It uses a Stack (S) to store intermediary values, an Environment (E) to store mappings from variables to values, a Control (C) to identify the code currently being executed, and a Dump (D) to store previous contexts of the machine to jump back later.

- **CEK Machine**: TODO
