module CodonSpec(runCodonSpec) where

import Test.Hspec
import Lib
import DataTypes

runCodonSpec :: () -> Spec
runCodonSpec _ = 
  describe "Parsing Codons:" $ do

    -- invalid length tests
    it "Parsing an empty string should return an InvalidLength error" $ do
      parseCodon "" (1,0) `shouldBe` CodonError (InvalidLength 1) ""

    it "Parsing a string with valid input should return a codon and the rest of the string" $ do
      parseCodon "AUU UGU GUG G" (1,0) `shouldBe` CodonResult (Codon "AUU") " UGU GUG G" (1,3)

    -- valid codon letters are A, U, G, C
    it "Using invalid letters should return an InvalidChars error" $ do
      parseCodon "A=U" (1,0) `shouldBe` CodonError (InvalidChars 1 2) "=U"
    
    it "Comment lines and spaces should be discarded" $ do
      parseCodon "     \n> flasj dflaksjdfl asjd flasj dfl\n> dlak jsdflasj dflkasj dfljsak l\n  UGU" (1,0) `shouldBe` 
          CodonResult (Codon "UGU") "" (4,5)
    
