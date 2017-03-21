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

To run a L1 application in the simulator use `./acqua-run` and send the program in the standard input. Check the options by running it without arguments to configure the architecture number of processing units and other options
