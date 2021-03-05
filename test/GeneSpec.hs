module GeneSpec(runGeneSpec) where

import Test.Hspec
import Lib
import DataTypes

runGeneSpec :: () -> Spec
runGeneSpec _ = 
  describe "Parsing a single gene:" $ do

    -- invalid length tests
    it "Parsing an empty string should return an invalid length error" $ do
      parseGene "" (1,0) `shouldBe` GeneError (InvalidLength 1) ""

    it "Procesing a string that is not the correct length should return an invalid length error" $ do
      parseGene "AAAU" (1,0) `shouldBe` GeneError (InvalidLength 1) ""

    -- valid codon letters are A, U, G, C
    -- valid case with 3 possible endings
    -- case END1:
    it "Parsing a string with valid input should return a gene and the rest of the input (Case END='UAG')" $ do
      parseGene "AUUUGUGUG UAG" (1,0) `shouldBe` GeneResult (FinishedGene 
                                            [Codon "AUU", Codon "UGU", Codon "GUG"] (Codon "UAG")) "" (1, 13)
    -- case END2:
    it "Parsing a string with valid input should return a gene and the rest of the input (Case END='UGA')" $ do
      parseGene "AUUUGUGUG UGA" (1,0) `shouldBe` GeneResult (FinishedGene 
                                            [Codon "AUU", Codon "UGU", Codon "GUG"] (Codon "UGA")) "" (1, 13)
    -- case END3:
    it "Parsing a string with valid input should return a gene and the rest of the input (Case END='UAA')" $ do
      parseGene "AUUUGUGUG UAA" (1,0) `shouldBe` GeneResult (FinishedGene 
                                            [Codon "AUU", Codon "UGU", Codon "GUG"] (Codon "UAA")) "" (1, 13)
    
    -- invalid characters tests
    it "Parsing a gene with invalid letters should return an InvalidChars error" $ do
      parseGene "A=U" (1,0) `shouldBe` GeneError (InvalidChars 1 2) "=U"
    
    -- invalid length
    it "Parsing a gene with no end codon should return an UnexpectedGeneEnd error" $ do
      parseGene "AAA AAA AAA" (1,0) `shouldBe` GeneError (UnexpectedGeneEnd 1) ""

    -- Redundant end codons test
    it "End codons after parsing a gene should be discarded" $ do
      parseGene "AUUUGUGUG UAA  UAG UGA UAG" (1,0) `shouldBe` GeneResult (FinishedGene 
                                            [Codon "AUU", Codon "UGU", Codon "GUG"] (Codon "UAA")) "" (1, 26)