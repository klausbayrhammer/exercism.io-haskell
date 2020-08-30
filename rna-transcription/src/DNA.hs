module DNA (toRNA) where

dnaRnaComplements :: Char -> Either Char Char
dnaRnaComplements 'C' = Right 'G'
dnaRnaComplements 'G' = Right 'C'
dnaRnaComplements 'T' = Right 'A'
dnaRnaComplements 'A' = Right 'U'
dnaRnaComplements x =  Left x

toRNA :: String -> Either Char String
toRNA xs = mapM dnaRnaComplements xs

