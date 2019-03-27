module Main where

import Scanner
import Parser
import AST

main :: IO ()
main = print $ parseGame teamList "team Jays: Jose, Kevin; team Yankees: John, Joe all with score called points"
