
# Probabilistic Functional Logic Programming (PFLP)

PFLP is a library for probabilistic programming in the functional logic
programming language [Curry](https://www.curry-lang.org/).
The library is based on the ideas presented in the paper
[Probabilistic Functional Logic Programming](https://doi.org/10.1017/S1471068419000085).

## Installation

This library can either be installed using the Curry package manager
[CPM](http://www.curry-lang.org/tools/cpm) which is part of recent
distributions of the Curry systems [PAKCS](https://www.curry-lang.org/pakcs/)
and [KiCS2](https://www.curry-lang.org/kics2/),
or it can be directly downloaded.

### Via CPM

First of all, make sure that your local package index is up-to-date.

    cypm update

You can then add `pflp` as a dependency to your own package. If you're
currently not working in a package, this command will add `pflp` as a
dependency to your distribution's home package instead. See the
[CPM manual](https://cpm.curry-lang.org/DOC/cpm-3.3.0/manual.pdf) for
details.

    cypm add --dependency pflp

Last but not least, you can install your package.

    cypm install

Afterwards, you should be able to import the library with the
following import statement.

```{.curry}
import PFLP
```

### Via Download

Download the source file `PFLP.curry` located in the `src` directory
and put it into the same directory as your own source file. You can
then import the library with the following import statement.

```{.curry}
import PFLP
```
