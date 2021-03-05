module Lib where

import Data.Either
-- import Data.Char
import DataTypes
import HelperFunctions

processFile :: String -> IO ()
processFile fileName = do
    content <- readFile fileName
    putStrLn ("Executing DNA sequencing on file " ++ fileName ++ "...")
    putStrLn ("Reading content: " ++ content)
    putStrLn "....\n"
    handleErrors content

-- This is the main function
handleErrors :: String -> IO ()
handleErrors input = case parseGenes input of
                        GenesError error input2 -> putStrLn (showError error input2)
                        GenesResult genes _ _   -> do   -- print results (one gene in a new line)
                                                        putStrLn "Parsing success:\n"
                                                        putStrLn "\n"
                                                        putStrLn (unlines (map show genes))

parseGenes :: Input -> GenesParseResult
parseGenes "" = GenesError (InvalidLength 1) "(empty input)"
parseGenes input = parseGenes2 input (1,0) ([]::Genes) -- initialize algorithm

parseGenes2 :: Input -> Position -> Genes -> GenesParseResult
parseGenes2 "" pos genes = GenesResult genes "" pos -- base case, return the list
parseGenes2 input pos genes = 
        case parseGene input pos of
            GeneError error input2 -> -- propagate error
                GenesError error input2
            GeneResult gene input2 pos2 -> -- add gene and continue to parse the rest of the input
                parseGenes2 input2 pos2 (genes ++ [gene]) 

parseGene :: Input -> Position -> GeneParseResult
parseGene "" pos = GeneError (InvalidLength (getLineOfPos pos)) ""
parseGene input pos = parseGene2 input pos (UnfinishedGene [])

parseGene2 :: Input -> Position -> Gene -> GeneParseResult
parseGene2 "" pos gene
    | isGeneFinished gene = GeneResult gene "" pos -- return the final gene
    | otherwise = GeneError (UnexpectedGeneEnd (getLineOfPos pos)) ""

parseGene2 input pos gene
    | isGeneFinished gene = let (input2, pos2) = discardEndCodons input pos in
                                GeneResult gene input2 pos2 -- return the complete gene with new position
    | otherwise =
        case parseCodon input pos of
            CodonError error input2 -> --propagate error
                GeneError error input2
            CodonResult codon input2 pos2-> -- call recursively
                parseGene2 input2 pos2 (addCodonToGene codon gene)

parseCodon :: Input -> Position -> CodonParseResult
parseCodon "" pos  = CodonError (InvalidLength (getLineOfPos pos)) ""
parseCodon input pos = parseCodon2 input pos (Codon "")

parseCodon2 :: Input -> Position -> Codon ->  CodonParseResult
parseCodon2 "" pos codon
    | isCodonFinished codon = CodonResult codon "" pos
    | otherwise = CodonError (InvalidLength (getLineOfPos pos)) ""

parseCodon2 input@(x:xs) pos codon@(Codon letts)
    | isCodonFinished codon = CodonResult codon input pos
    | isCommentLine input = parseCodon2 (discardLine input) (sumOneLine pos) codon
    | isReturn x = parseCodon2 xs (sumOneLine pos) codon
    | isSpace x = parseCodon2 xs (sumOneCol pos) codon
    | isValidCodonLetter x = parseCodon2 xs (sumOneCol pos) (Codon (letts++[x]))
    | otherwise = CodonError (InvalidChars (getLineOfPos pos) (1 + getColOfPos pos)) (take 20 input)
