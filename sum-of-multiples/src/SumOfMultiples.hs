module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum multiples
    where
        multiples = filter isMultipleOfFactors [1..limit-1]
        isMultipleOfFactors potentialMultiple = any (\factor -> rem potentialMultiple factor == 0) factorsWithoutZero
        factorsWithoutZero = filter (>0) factors
