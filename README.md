![lollipop logo](http://malmqvist.it/lollipop5.png)

lollipop is a general purpose, functional programming language with support for linear types. This is a bachelor thesis project at Chalmers University of Technology, Gothenburg.

### Goal
The main goal is to develop a proof-of-concept of linear types in a basic functional language. This is to enable easy access to the concept for developers that are interested in learning about linear types, in a practical environment.

### Running programs in lollipop
The read-eval-print-loop, REPL (Repl.hs) is used to load and run programs written in lollipop (.lp).

#### Requirements
The REPL is built in Haskell and therefore requires GHC to work. A recommendation is to install GHC's interactive environment, GHCi.

#### Running the REPL
To run the read-eval-print-loop, fire up a terminal, move to the lollipop root directory and execute

    runghc -iAST/:grammar/ Repl.hs

The loading of the REPL will take some time and when the it's done you'll see:

    >

From here you can load programs (ending with .lp) by using `:l`, reloading programs using `:r` and leaving the REPL by `:q`.

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
    [6,8,19]

#### Disclaimer
As for now the REPL is in a beta-phase and syntax-errors and unsuccessfully loaded programs can cause it to crash easily. If this happens, simply restart by writing    

    runghc -iAST/:grammar/ Repl.hs

In rare cases the REPL gets stuck in an endless evaluation loop. This can happen when trying to run functions that cannot be found in the variable-value-environment, i.e when you're calling a function that doesn't exist. If the REPL gets stuck in a loop, try killing the process using ctrl+c, or by killing the ghc-process.
