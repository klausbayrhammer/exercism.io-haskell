module SumOfMultiples (sumOfMultiples) where

import Data.Set (fromList)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum multiples
    where
        multiples = fromList factorsUntilLimit
        factorsUntilLimit = concat $ map multiplesOfFactorUntilLimit factorsWithoutZero
        multiplesOfFactorUntilLimit factor = takeWhile (< limit) [factor, 2 * factor ..]
        factorsWithoutZero = filter (>0) factors
