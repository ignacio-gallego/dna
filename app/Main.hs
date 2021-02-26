module Main where

import Lib
import DataTypes
import HelperFunctions

main :: IO ()
main =  do
    putStrLn "Enter filename to parse:"
    fileName <- getLine
    processFile fileName 
