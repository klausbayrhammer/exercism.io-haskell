module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = isPangramRec ['a'..'z']  (map toLower text)

isPangramRec :: [Char] -> String -> Bool
isPangramRec [] _ = True
isPangramRec (charToTest:remainingCharsToTest) text = elem charToTest text && isPangramRec remainingCharsToTest text