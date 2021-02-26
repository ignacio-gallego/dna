module DataTypes where

newtype Codon = Codon String deriving (Eq, Show)
data Gene =  FinishedGene [Codon] Codon | UnfinishedGene [Codon] deriving (Eq, Show)
data Error = InvalidChars | InvalidLength | UnexpectedGeneEnd deriving (Eq, Show)
type Genes = [Gene]
type GenesParseResult = Either Genes Error
type GeneParseResult  = Either Gene Error
type CodonParseResult = Either Codon Error
