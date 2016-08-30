[![Build Status](https://travis-ci.org/fotanus/acqua.svg?branch=master)](https://travis-ci.org/fotanus/acqua)

Acqua
-----

To compile and test:

```
cabal configure --enable-tests
cabal install --only-dependencies
cabal build
cabal test
```

Executables are linked in the root folder

To compile an l1 program and display the IR:

```
./acqua-ir < program.l1
```

To show the L1 AST of a program

```
./l1-ast < program.l1
```

To typecheck an L1 program

```
./l1-typecheck < program.l1
```

To run a L1 application in the simulator

```
./acqua-run 10 0 x 3 < program.l1
```

Where `10` is the number of PUs, `0` is the number of steps to exchange a message (used to simulate hierarchical crossbar), `x` is the variable name and `3` is the parameter value. To apply a map add more number as parameters.
