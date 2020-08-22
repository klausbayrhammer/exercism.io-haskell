module Bob (responseFor) where

import Data.Char (toUpper, toLower, isSpace)

responseFor :: String -> String
responseFor xs
 | isSilence trimmedText = "Fine. Be that way!"
 | isForcefulQuestion trimmedText = "Calm down, I know what I'm doing!"
 | isQuestion trimmedText = "Sure."
 | isShouting trimmedText = "Whoa, chill out!"
 | otherwise = "Whatever."
    where
        trimmedText = trim xs

isShouting :: String -> Bool
isShouting x = isUpperCaseEqual x && (isLowerCaseNotEqual x)

isUpperCaseEqual :: String -> Bool
isUpperCaseEqual x = map toUpper x == x

isLowerCaseNotEqual :: String -> Bool
isLowerCaseNotEqual x = map toLower x /= x

isQuestion :: String -> Bool
isQuestion x = last x == '?'

isSilence :: String -> Bool
isSilence x = x == []

isForcefulQuestion :: String -> Bool
isForcefulQuestion x = isQuestion x && isShouting x

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace