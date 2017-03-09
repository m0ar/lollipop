![lollipop logo](logo.png)

lollipop is a general purpose, functional programming language with support for linear types. This is a bachelor thesis project at Chalmers University of Technology, Gothenburg.

### Goal
The main goal is to develop a proof-of-concept of linear types in a basic functional language. This is to enable easy access to the concept for developers that are interested in learning about linear types, in a practical environment.

The project thesis can be found [HERE!](https://github.com/m0ar/lollipop/blob/develop/Lollipop_publish.pdf)

### Running programs in lollipop
The lollipop interpreter, loli, (loli.hs) is used to load and run programs written in lollipop (.lp).

#### Requirements
The lollipop interpreter is built in Haskell and therefore requires the Glasgow Haskell Compiler, GHC to run.

#### Running the lollipop interpreter
To run the lollipop interpreter, fire up a terminal, move to the lollipop root directory and execute

    runghc -iAST/:grammar/ loli.hs

It will take some time loading the interpreter, but when it's done the terminal will prompt:

    >

From here you can load lollipop programs (ending with .lp) by using `:l`, reloading programs using `:r` and leaving the interpreter by `:q`.

E.g: Loading of the program sugar (filename sugar.lp):

    >:l sugar
    Successfully loaded sugar
    sugar>

From here you can execute functions and expressions in the loaded program

E.g: Running some basic functions in sugar

     >:l sugar
    Successfully loaded sugar
    sugar> fac 5
    120
    sugar> head [7,8,9]
    7
    sugar> map (\x -> x+2) [4,6,8]
    [6,8,10]

#### Disclaimer
As for now the lollipop interpreter is in a beta-phase and syntax-errors and unsuccessfully loaded programs can cause it to crash. In this case, restart it using the same command again:

    runghc -iAST/:grammar/ loli.hs

In rare cases the interpreter gets stuck in an evaluation loop. If the interpreter does this, try ending the process using ctrl+c, or by killing the ghc-process.
