module Main where

import Test.Hspec
import GenesSpec
import GeneSpec
import CodonSpec

main :: IO ()
main = hspec $ do
  runCodonSpec()
  runGeneSpec()
  runGenesSpec()