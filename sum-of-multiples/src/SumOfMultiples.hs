module SumOfMultiples (sumOfMultiples) where

import Data.Set (fromList, union, empty)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum multiples
    where
        multiples = foldl addFactors empty $ filter (>0) factors
        addFactors acc factor = union acc $ fromList [factor, 2 * factor .. limit-1]
