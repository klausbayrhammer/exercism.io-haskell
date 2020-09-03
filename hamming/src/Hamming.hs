module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | areStrandsNotEqual = Nothing
    | otherwise = Just $ length $ filter isTupleNotEqual zippedStrands
    where
        zippedStrands = zip xs ys
        isTupleNotEqual (a, b) = a /= b
        areStrandsNotEqual = length xs /= (length ys)
