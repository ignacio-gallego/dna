module Lib where

import Data.Either
import Data.Char
import DataTypes
import HelperFunctions

processFile :: String -> IO ()
processFile fileName = do
    content <- readFile fileName
    putStrLn ("Executing DNA sequencing on file " ++ fileName ++ "...")
    putStrLn ("Reading content: " ++ content)
    putStrLn "Result:\n"
    putStrLn (show(parseGenes(content)))

-- This is the main function
parseGenes :: String -> GenesParseResult
parseGenes "" = Right InvalidLength
parseGenes input = parseGenes2 [] input

parseGenes2 :: Genes -> String -> GenesParseResult
parseGenes2 genes "" = Left genes -- base case, return the list
parseGenes2 genes input = 
    let (geneRes, input2) = parseGene input in
        case geneRes of
            Right error -> -- propagate error
                Right error
            Left gene -> -- add gene and continue to parse the rest of the input
                parseGenes2 (genes ++ [gene]) input2

parseGene :: String -> (GeneParseResult, String)
parseGene "" = (Right InvalidLength , "")
parseGene str = parseGene2 (UnfinishedGene []) str

parseGene2 :: Gene -> String -> (GeneParseResult, String)
parseGene2 gene ""
    | isGeneFinished gene = (Left gene, "")
    | otherwise = (Right UnexpectedGeneEnd, "")

parseGene2 gene input
    | isGeneFinished gene = (Left gene, discardEndCodons input) -- return the complete gene
    | otherwise =
     let (codonRes, input2) = parseCodon input in
        case codonRes of
            Right error -> --propagate error
                (Right error, input2)
            Left codon -> -- call recursively
                parseGene2 (addCodonToGene gene codon) input2

parseCodon :: String -> (CodonParseResult, String)
parseCodon [] = (Right InvalidLength, "")
parseCodon str = parseCodon2 (Codon "") str

parseCodon2 :: Codon -> String -> (CodonParseResult, String)
parseCodon2 codon ""
    | isCodonFinished codon = (Left codon, "")
    | otherwise = (Right InvalidLength, "")

parseCodon2 codon@(Codon letts) str@(x:xs)
    | isCodonFinished codon = (Left codon, str)
    | isCommentLine str = parseCodon2 codon (discardLine str)
    | isSpace x = parseCodon2 codon xs
    | isValidCodonLetter x = parseCodon2 (Codon (letts++[x])) xs
    | otherwise = (Right InvalidChars, str)
