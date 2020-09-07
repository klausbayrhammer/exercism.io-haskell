module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
    | n > 0 && n < 65 = Just (squareRec n)
    | otherwise = Nothing

squareRec :: Integer -> Integer
squareRec n
    | n == 1 = 1
    | otherwise = 2 * squareRec (n-1)

total :: Integer
total = sum $ map squareRec [1..64]
