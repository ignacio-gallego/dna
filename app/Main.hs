module Main where

import Lib
import DataTypes
import HelperFunctions
import Data.List

main :: IO ()
main =  do
    putStrLn "Enter filename to parse (ENTER for default input file):"
    fileName <- myGetLine
    processFile fileName 

myGetLine :: IO String
myGetLine = do
    c <- getChar
    case c of
        '\n' -> return "sample-input.dna" -- default input file of project
        other -> do
            fileName <- getLine
            return (other : fileName)