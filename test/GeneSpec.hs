module GeneSpec(runGeneSpec) where

import Test.Hspec
import Lib
import DataTypes

runGeneSpec :: () -> Spec
runGeneSpec _ = 
  describe "Parsing a single gene:" $ do

    -- invalid length tests
    it "Parsing an empty string should return an invalid length error" $ do
      fst(parseGene "") `shouldBe` Right InvalidLength

    it "Procesing a string that is not the correct length should return an invalid length error" $ do
      fst(parseGene "AAAU") `shouldBe` Right InvalidLength

    -- valid codon letters are A, U, G, C
    -- valid case with 3 possible endings
    -- case END1:
    it "Parsing a string with valid input should return a gene and the rest of the input (Case END='UAG')" $ do
      parseGene "AUUUGUGUG UAG" `shouldBe` (Left (FinishedGene 
                                            [Codon "AUU", Codon "UGU", Codon "GUG"] (Codon "UAG")), "")
    -- case END2:
    it "Parsing a string with valid input should return a gene and the rest of the input (Case END='UGA')" $ do
      parseGene "AUUUGUGUG UGA" `shouldBe` (Left (FinishedGene 
                                            [Codon "AUU", Codon "UGU", Codon "GUG"] (Codon "UGA")), "")
    -- case END3:
    it "Parsing a string with valid input should return a gene and the rest of the input (Case END='UAA')" $ do
      parseGene "AUUUGUGUG UAA" `shouldBe` (Left (FinishedGene 
                                            [Codon "AUU", Codon "UGU", Codon "GUG"] (Codon "UAA")), "")
    
    -- invalid characters tests
    it "Parsing a gene with invalid letters should return an InvalidChars error" $ do
      fst(parseGene "A=U") `shouldBe` Right InvalidChars
    
    -- invalid length
    it "Parsing a gene with no end codon should return an UnexpectedGeneEnd error" $ do
      fst(parseGene "AAA AAA AAA") `shouldBe` Right UnexpectedGeneEnd 
