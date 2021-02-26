module GenesSpec(runGenesSpec) where

import Test.Hspec
import Lib
import DataTypes
import Data.Either

runGenesSpec :: () -> Spec
runGenesSpec _ = 
  describe "Parsing sequences of genes:" $ do

    -- invalid length tests
    it "Sequencing an empty string should return an invalid length error" $ do
      parseGenes "" `shouldBe` Right InvalidLength

    it "Sequencing a string that is not the correct length should return an invalid length error" $ do
      parseGenes "AAAU" `shouldBe` Right InvalidLength

    -- valid codon letters are A, U, G, C
    -- valid case with 3 possible endings
    it "Sequencing 3 endings of codons in 3 genes should return 3 genes (structure)" $ do
      fromLeft [] (parseGenes "AUU UUU UGU GUG UAG UAG UGA AUU UGU GUG UGA UGA UGA  AUU UGU GUG UAA") `shouldBe` 
        [FinishedGene [Codon "AUU",Codon "UUU", Codon "UGU",Codon "GUG"] (Codon "UAG"),
         FinishedGene [Codon "AUU",Codon "UGU",Codon "GUG"] (Codon "UGA"),
         FinishedGene [Codon "AUU",Codon "UGU",Codon "GUG"] (Codon "UAA")]
    
    it "Sequencing 3 endings of codons in 3 genes should return 3 genes (list length)" $ do
      length(fromLeft [] (parseGenes "AUU UUU UGU GUG UAG UAG UGA AUU UGU GUG UGA UGA UGA  AUU UGU GUG UAA")) `shouldBe`  3
    
    -- invalid characters tests
    it "Parsing a gene with invalid letters should return an InvalidChars error" $ do
      parseGenes "ATTTTTU" `shouldBe` Right InvalidChars
    
    -- invalid length
    it "Parsing a gene with no end codon should return an UnexpectedGeneEnd error" $ do
      parseGenes "AAA AAA AAA UGA UUU" `shouldBe` Right UnexpectedGeneEnd 
