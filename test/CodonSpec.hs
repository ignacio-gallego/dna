module CodonSpec(runCodonSpec) where

import Test.Hspec
import Lib
import DataTypes

runCodonSpec :: () -> Spec
runCodonSpec _ = 
  describe "Parsing Codons:" $ do

    -- invalid length tests
    it "Parsing an empty string should return an invalid length error" $ do
      fst(parseCodon "") `shouldBe` Right InvalidLength

    it "Parsing a string with valid input should return a codonn and the rest of the string" $ do
      parseCodon "AUUUGUGUGG" `shouldBe` (Left (Codon "AUU"), "UGUGUGG")

    -- valid codon letters are A, U, G, C
    it "Using invalid letters should return an InvalidChars error" $ do
      fst(parseCodon "A=U") `shouldBe` Right InvalidChars
    
    