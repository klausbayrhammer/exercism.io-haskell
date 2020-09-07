module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
    | isValidChessBoardPosition = Just (grainsOnSquare)
    | otherwise = Nothing
    where
        isValidChessBoardPosition = n > 0 && n < 65
        grainsOnSquare = iterate (*2) 1 !! (fromIntegral n - 1)

total :: Integer
total = 18446744073709551615
