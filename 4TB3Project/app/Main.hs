module Main where

import Scanner
import Parser (parseGame, game)
import AST
import PreCompiler (preCompile)
import Compiler (compileGame)

import Text.PrettyPrint.HughesPJ (render)
import System.IO 

main :: IO ()
main = generateGame exGames

generateGame :: [String] -> IO ()
generateGame [] = return ()
generateGame (x:xs) = do
    contents <- readFile ("../Examples/" ++ x ++ ".txt")
    writeFile ("Examples/" ++ x ++ ".py") (render $ compileGame $ preCompile $ parseGame game contents)
    generateGame xs

-- | List of filenames for the example games
exGames :: [String]
exGames = ["tennis", "baseball", "survivor", "bigbrother", "thegenius", "original"]
