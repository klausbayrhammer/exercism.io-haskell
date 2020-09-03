module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, empty, insertWith)
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = foldM aggregateNucleotides empty xs

aggregateNucleotides :: Map Nucleotide Int -> Char -> Either String (Map Nucleotide Int)
aggregateNucleotides acc dna = increaseNucleotideCount <$> mappedDna
    where
        increaseNucleotideCount nucleotide = insertWith (+) nucleotide 1 acc
        mappedDna = mapNucleotide dna

mapNucleotide :: Char -> Either [Char] Nucleotide
mapNucleotide 'A' = Right A
mapNucleotide 'C' = Right C
mapNucleotide 'G' = Right G
mapNucleotide 'T' = Right T
mapNucleotide x = Left [x]