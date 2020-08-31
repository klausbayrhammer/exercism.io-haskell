module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
    | containsInvalidChars = Left "Contains invalid chars"
    | otherwise = Right (fromList nucleotideCountsTuples)
    where
        containsInvalidChars = any (\x -> not (elem x ['G', 'C', 'T', 'A'])) xs
        occurences nucleotide = length (filter (== nucleotide) xs)
        nucleotideCountsTuples =[
                (G, occurences 'G'),
                (C, occurences 'C'),
                (A, occurences 'A'),
                (T, occurences 'T')
            ]
