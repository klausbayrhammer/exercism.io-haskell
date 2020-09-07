module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
    | isValidChessBoardPosition = Just (iterate (*2) 1 !! (fromIntegral n - 1))
    | otherwise = Nothing
    where
        isValidChessBoardPosition = n > 0 && n < 65

total :: Integer
total = 18446744073709551615
