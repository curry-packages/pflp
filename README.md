
# Probabilistic Functional Logic Programming (PFLP)

PFLP is a library for probabilistic programming in the functional logic programming language [Curry](https://www-ps.informatik.uni-kiel.de/currywiki/).
The library is based on the ideas presented in the paper [Probabilistic Functional Logic Programming](https://www-ps.informatik.uni-kiel.de/~sad/padl2018-preprint.pdf).

## Installation

This library can either be installed using the Curry package manager [CPM](https://www-ps.informatik.uni-kiel.de/currywiki/tools/cpm) which is already part of recent distributions of the Curry systems [PAKCS](https://www.informatik.uni-kiel.de/~pakcs/index.html) and [KiCS2](https://www-ps.informatik.uni-kiel.de/kics2/), or it can be directly downloaded.

### Via CPM

First of all, make sure that your local package index is up-to-date.

    cypm update

You can then add `pflp` as a dependency to your own package. If you're currently not working in a package, this command will add `pflp` as a dependency to your distribution's home package instead. See the [CPM manual](https://www-ps.informatik.uni-kiel.de/currywiki/_media/tools/cpm/manual.pdf) for details. 

    cypm add --dependency pflp

Last but not least, you can install your package.

    cypm install

Afterwards, you should be able to import the library with the following import statement.

```{.curry}
import PFLP
```

### Via Download

Download the source file `PFLP.curry` located in the `src` directory and put it into the same directory as your own source file. You can then import the library with the following import statement.

```{.curry}
import PFLP
```
