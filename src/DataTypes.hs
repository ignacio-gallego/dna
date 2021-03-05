module DataTypes where

-- types Codon and Gene
newtype Codon = Codon String deriving (Eq, Show)
data Gene =  FinishedGene [Codon] Codon | UnfinishedGene [Codon] deriving (Eq, Show)
type Genes = [Gene]

-- Position for error information
type Position = (Line, Column)
type Column = Int
type Line = Int

-- Error data type
data Error = InvalidChars Line Column | InvalidLength Line | UnexpectedGeneEnd Line deriving (Eq, Show)

-- String input 
type Input = String
type RemainingInput = Input
type ErrorLocationInput = Input

-- Result types 
data CodonParseResult = CodonResult Codon RemainingInput Position | 
                        CodonError Error ErrorLocationInput deriving (Eq, Show)

data GeneParseResult  = GeneResult Gene RemainingInput Position |
                        GeneError Error ErrorLocationInput deriving (Eq, Show)

data GenesParseResult = GenesResult Genes RemainingInput Position |
                        GenesError Error ErrorLocationInput deriving (Eq, Show)
