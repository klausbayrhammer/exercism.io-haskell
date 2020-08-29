module DNA (toRNA) where
import Data.List (find)
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Data.Either.Combinators (fromRight', fromLeft')

dnaRnaComplements :: Char -> Either Char Char
dnaRnaComplements 'C' = Right 'G'
dnaRnaComplements 'G' = Right 'C'
dnaRnaComplements 'T' = Right 'A'
dnaRnaComplements 'A' = Right 'U'
dnaRnaComplements x =  Left x

toRNA :: String -> Either Char String
toRNA xs
    | any isLeft potentialRna = Left (fromLeft' (fromJust (find isLeft potentialRna)))
    | otherwise = Right (map fromRight' potentialRna)
    where
        potentialRna = map dnaRnaComplements xs
