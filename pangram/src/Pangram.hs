module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = isPangramRec ['a'..'z']  (map toLower text)

isPangramRec :: [Char] -> String -> Bool
isPangramRec [] _ = True
isPangramRec (x:xs) text = elem x text && isPangramRec xs text