module Pangram (isPangram) where

import Data.List (nub)
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = length (uniqueLowercaseCharsFromAToZ text) == length relevantLetters

relevantLetters :: [Char]
relevantLetters = ['a'..'z']

uniqueLowercaseCharsFromAToZ :: String -> String
uniqueLowercaseCharsFromAToZ text = nub (filterCharsFromAToZ (toLowerCase text))

filterCharsFromAToZ :: String -> String
filterCharsFromAToZ string = filter isNumberFromAToZ string

isNumberFromAToZ :: Char -> Bool
isNumberFromAToZ test = elem test relevantLetters

toLowerCase :: String -> String
toLowerCase mixedCaseString = map toLower mixedCaseString
