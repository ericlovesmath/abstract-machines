# Abstract Machines

Abstract machines for functional programming languages

## Overview

* Stephan Diehl, Pieter Hartel, Peter Sestoft (2000)
  Abstract machines for programming language implementation
  [Link](http://www.inf.ed.ac.uk/teaching/courses/lsi/diehl_abstract_machines.pdf)

## Books

* Simon Peyton Jones (1987), The Implementation of Functional Programming Languages
  [Link](https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/)

* Simon Peyton Jones (1992), Implementing functional languages: A tutorial
  [Link](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/)

* Werner Kluge (2004), Abstract Computing Machines: A Lambda Calculus Perspective
  [Link](https://www.springer.com/de/book/9783540211464)

## Other

* R.J.M. Hughes (1982), Super-combinators: A new implementation method for applicative languages
  [Link](https://dl.acm.org/citation.cfm?id=802129)
  > The key to the new approach is to generalise the class of combinators [W.r.t David Turner's approach of compiling lambda expressions to SKI]. Recall that S, K, and I can be
  > defined by lambda-expressions (...).
  > These lambda-expressions have two special properties that make them suitable for use directly as operators in a graph
  > reduction machine. Firstly, they have no free variables and so are "pure code", hence their internal structur is of no consequence
  > and any suitable representation may be used for them.
  > Secondly, their bodies are applicative forms, ie are composed from variables and constants by applications.
  > ...
  > Any lambda-expression with these two properties is a combinator (...)
  > Where it is necessary to distinguish generalised combinators from Turner's, they are called super-combinators.

* R.J.M. Hughes (1982), Graph-Reduction with Super-Combinators
  [Link](https://www.cs.ox.ac.uk/publications/publication3771-abstract.html)

## Strict languages

### CEK/CESK

* Matt Might blog post [Link](http://matt.might.net/articles/cek-machines/)

* dito [Link](http://matt.might.net/articles/cesk-machines/)

### SECD Machine

* Landin (1964), The mechanical evaluation of expressions
  [Link](https://www.cs.cmu.edu/~crary/819-f09/Landin64.pdf)

### Functional Abstract Machine (FAM)

* Luca Cardelli (1983), The functional abstract machine, Technical Report
  [Link](http://lucacardelli.name/Papers/FAM.pdf)

* Luca Cardelli (1984), Compiling a functional language
  [Link](http://lucacardelli.name/Papers/CompilingML.A4.pdf)

### Categorical Abstract Machine (CAM)

* Cousineau, Curien, Mauny (1985), The Categorical Abstract Machine
  [Link](https://www.sciencedirect.com/science/article/pii/0167642387900207)

### Zinc Abstract Machine

* Xavier Leroy (1990), The Zinc experiment: An economical implementation of the ML language
  [Link](https://xavierleroy.org/publi/ZINC.pdf)
  
## Lazy Languages

### SK Machine

* David Turner (1979), A new implementation technique for applicative languages
  [Link](https://onlinelibrary.wiley.com/doi/abs/10.1002/spe.4380090105)

### G-Machine

* L. Augustsson (1984), A compiler for lazy ML
  [Link](https://dl.acm.org/citation.cfm?id=802038)

* T. Johnsson (1984), Efficient compilation of lazy evaluation
  [Link](https://dl.acm.org/citation.cfm?id=502880)

### Krivine Machine

* Remi Douence, Pascal Fradet (2007), The next 700 Krivine machines
  [Link](https://link.springer.com/article/10.1007/s10990-007-9016-y)

* Jean-Louis Krivine (2007), A call-by-name lambda-calculus machine
  [Link](http://www.pps.univ-paris-diderot.fr/~krivine/articles/lazymach.pdf)

### Three Instruction Machine (TIM)

* J. Fairbairn, S.C. Wray (1987), TIM: A simple lazy abstract machine to execute supercombinators
  [Link](https://link.springer.com/chapter/10.1007/3-540-18317-5_3)

* G. Argo (1989), Improving the three instruction machine
  [Link](https://dl.acm.org/citation.cfm?id=99370.99378)

### Spineless Tagless G-Machine (STG)

* Simon Peyton Jones (1992), Implementing lazy functional languages on stock hardware, the spineless tagless G-Machine
  [Link](https://www.microsoft.com/en-us/research/publication/implementing-lazy-functional-languages-on-stock-hardware-the-spineless-tagless-g-machine/)

* STGi Implementation on Hackage
  [Link](http://hackage.haskell.org/package/stgi)
