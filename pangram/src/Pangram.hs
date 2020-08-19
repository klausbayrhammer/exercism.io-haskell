module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all (\charToTest -> elem charToTest lowerCaseText) ['a'..'z']
    where
        lowerCaseText = map toLower text