module HelperFunctions where

import DataTypes
import Data.Char

isGeneFinished :: Gene -> Bool
isGeneFinished (FinishedGene _ _) = True
isGeneFinished (UnfinishedGene _) = False

addCodonToGene :: Gene -> Codon -> Gene 
addCodonToGene gene@(UnfinishedGene cs) codon
    | isStopCodon codon = FinishedGene cs codon
    | otherwise = UnfinishedGene (cs ++ [codon])
addCodonToGene gene@(FinishedGene _ _) codon = gene -- add codon to finished gene has no efect

isStopCodon :: Codon -> Bool
isStopCodon (Codon xs) = isStopString xs

isStopString :: String -> Bool
isStopString xs = xs == "UAG" || xs == "UGA" || xs == "UAA"

isCodonFinished :: Codon -> Bool
isCodonFinished (Codon xs) = length xs == 3

isValidCodonLetter :: Char -> Bool
isValidCodonLetter x = x `elem`  "AUGCaugc"

isCommentLine :: String -> Bool
isCommentLine [] = False
isCommentLine (x:xs) = x == '>'

discardLine :: String -> String
discardLine = dropWhile (/= '\n')

discardEndCodons :: String -> String
discardEndCodons "" = ""
discardEndCodons input@(x:y:z:xs)
    | isSpace x = discardEndCodons (y:z:xs)
    | isStopString [x, y, z] = discardEndCodons xs
    | otherwise = input
discardEndCodons input@(x:xs)
    | isSpace x = discardEndCodons xs
    | otherwise = input