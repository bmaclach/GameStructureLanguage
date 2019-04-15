# 4TB3Project

To build the software and run the examples, run `cabal run`. The examples will be built to the Examples/ directory. To run an example, navigate to the Examples directory and run `python examplename.py`, replacing "examplename" with the filename for the example you want to run. Note that all examples have been tested with Python 3, and there are no guarantees as to whether they will work with Python 2.

To run the tests, run `cabal test`.

To build the documentation, run `cabal haddock`. The documentation will be built to dist/doc/html.

- src: Contains the modules defining the AST, scanner, parser, and compiler for the language.
- app: Contains the main function which builds the examples
- test: Contains the tests
- Examples: Contains gamelib.py, a library of common functions and classes needed for the games. Also contains the examples after they have been built.