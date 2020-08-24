module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz initialNumber
    | initialNumber <= 0 = Nothing
    | otherwise = Just (collatzRec initialNumber 0)

collatzRec :: Integer -> Integer -> Integer
collatzRec initialNumber numberOfSteps
    | isOne = numberOfSteps
    | isEven = collatzRec (div initialNumber 2) (numberOfSteps + 1)
    | otherwise = collatzRec (initialNumber * 3 + 1) (numberOfSteps + 1)
    where
        isOne = initialNumber == 1
        isEven = rem initialNumber 2 == 0
