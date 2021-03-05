module HelperFunctions where

import DataTypes
-- import Data.Char

isGeneFinished :: Gene -> Bool
isGeneFinished (FinishedGene _ _) = True
isGeneFinished (UnfinishedGene _) = False

addCodonToGene :: Codon -> Gene -> Gene 
addCodonToGene codon gene@(UnfinishedGene cs)
    | isStopCodon codon = FinishedGene cs codon
    | otherwise = UnfinishedGene (cs ++ [codon])
addCodonToGene codon gene@(FinishedGene _ _) = gene -- add codon to finished gene has no efect

isStopCodon :: Codon -> Bool
isStopCodon (Codon xs) = isStopString xs

isStopString :: String -> Bool
isStopString xs = xs == "UAG" || xs == "UGA" || xs == "UAA"

isCodonFinished :: Codon -> Bool
isCodonFinished (Codon xs) = length xs == 3

isValidCodonLetter :: Char -> Bool
isValidCodonLetter x = x `elem`  "AUGCaugc"

isCommentLine :: Input -> Bool
isCommentLine "" = False
isCommentLine (x:xs) = x == '>'

discardLine :: Input -> RemainingInput
discardLine xs = tail (dropWhile (/= '\n') xs)

discardEndCodons :: Input -> Position -> (RemainingInput, Position)
discardEndCodons "" pos = ("", pos)
discardEndCodons input@(x:y:z:xs) pos
    | isSpace x = discardEndCodons (y:z:xs) (sumOneCol pos)
    | isReturn x = discardEndCodons (y:z:xs) (sumOneLine pos)
    | isStopString [x, y, z] = discardEndCodons xs (sumCols 3 pos)
    | otherwise = (input, pos)
discardEndCodons input@(x:xs) pos
    | isSpace x = discardEndCodons xs (sumOneCol pos)
    | isReturn x = discardEndCodons xs (sumOneLine pos)
    | otherwise = (input, pos)

isSpace :: Char -> Bool 
isSpace x = x == '\t' || x == ' '

isReturn :: Char -> Bool
isReturn x = x == '\n'

showError :: Error -> ErrorLocationInput -> String
showError (InvalidChars line col) input = "Invalid character error: there is an invalid character in line " ++ show line ++
                                            " at column " ++ show col ++ ". Location of error: " ++ filter (/='\n') input
showError (UnexpectedGeneEnd line) input = "Unexpected end of input: input ended without a final codon while parsing current gene at line "
                                            ++ show line
showError (InvalidLength line) input = "Invalid length error: cannot parse a codon because there is not enough characters at the end " ++
                                            "of the input at line " ++ show line

getLineOfPos :: Position -> Line
getLineOfPos = fst

getColOfPos :: Position -> Column 
getColOfPos = snd

sumOneLine :: Position -> Position
sumOneLine (line, col) = (line + 1, 0)

sumCols :: Int -> Position -> Position 
sumCols cols (line, col) = (line, col + cols)

sumOneCol :: Position -> Position
sumOneCol = sumCols 1
