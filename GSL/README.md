# Game Structure Language

To build the software and run the examples, run `cabal run GSL-exe`. The examples will be built to the Examples/ directory. To run an example, navigate to the Examples directory and run `python examplename.py`, replacing "examplename" with the filename for the example you want to run. Note that all examples have been tested with Python 3, and they are not guaranteed to work with Python 2.

To run the tests, run `cabal test`.

To build the documentation, run `cabal haddock`.

- src: Contains the modules defining the AST, scanner, parser, and compiler for the language.
- app: Contains the main function which builds the examples
- test: Contains the tests
- Examples: Contains gamelib.py, a library of common functions and classes needed for the games. Also contains the examples after they have been built.