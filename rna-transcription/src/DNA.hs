module DNA (toRNA) where
import Data.Either (partitionEithers)

dnaRnaComplements :: Char -> Either Char Char
dnaRnaComplements 'C' = Right 'G'
dnaRnaComplements 'G' = Right 'C'
dnaRnaComplements 'T' = Right 'A'
dnaRnaComplements 'A' = Right 'U'
dnaRnaComplements x =  Left x

toRNA :: String -> Either Char String
toRNA xs
    | hasErrors = Left (head bad)
    | otherwise = Right good
    where
        (bad, good) = partitionEithers (map dnaRnaComplements xs)
        hasErrors = length bad > 0

