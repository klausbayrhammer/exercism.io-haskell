module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList)
import Data.Either.Combinators  (mapRight)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

mapNucleotide :: Char -> Either [Char] Nucleotide
mapNucleotide 'A' = Right A
mapNucleotide 'C' = Right C
mapNucleotide 'G' = Right G
mapNucleotide 'T' = Right T
mapNucleotide x = Left [x]

countNucleotides :: [Nucleotide] -> Map Nucleotide Int
countNucleotides xs = fromList (map countOccurences [A, C, G, T])
    where
        countOccurences nucleotide = (nucleotide, length (filter (== nucleotide) xs))

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = mapRight countNucleotides (mapM mapNucleotide xs)