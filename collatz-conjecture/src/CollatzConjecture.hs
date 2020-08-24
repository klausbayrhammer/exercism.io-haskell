module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz initialNumber
    | isZeroOrNegative = Nothing
    | otherwise = Just (collatzRec initialNumber)
    where
        isZeroOrNegative = initialNumber <= 0

collatzRec :: Integer -> Integer
collatzRec initialNumber
    | isOne = 0
    | isEven = 1 + collatzRec (div initialNumber 2)
    | otherwise = 1 + collatzRec (initialNumber * 3 + 1)
    where
        isOne = initialNumber == 1
        isEven = rem initialNumber 2 == 0
