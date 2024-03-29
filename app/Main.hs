
module Main where

import System.IO
import System.Console.Isocline

import Exp
import Parsing
import Printing
import REPLCommand
import Text.ParserCombinators.Parsec(parse)

import Eval
import Sugar

main :: IO ()
main = do
    setHistory "history.txt" 200
    input <- readline "miniHaskell"
    case parse replCommand "<input>" input of
      Left err -> print err >> main
      Right cmd -> case cmd of
                    Quit -> return ()
                    Load s -> putStrLn "notImplemented" >> main
                    Eval l -> case parse exprParser "<input>" l of
                        Left err -> print err >> main
                        Right c -> putStrLn ( showExp c ) >> main
                        

