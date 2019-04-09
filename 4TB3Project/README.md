# 4TB3Project

To build the software and run the examples, run `cabal run`. The examples will be built to the Example/ directory.

To run the tests, run `cabal test`.

To build the documentation, run `cabal haddock`. The documentation will be built to dist/doc/html.

- src: Contains the modules defining the AST, scanner, parser, and compiler for the language.
- app: Contains the main function which builds the examples
- test: Contains the tests
- Examples: Contains gamelib.py, a library of common functions and classes needed for the games. Also contains the examples after they have been built.