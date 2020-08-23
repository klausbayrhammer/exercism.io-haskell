module Bob (responseFor) where

import Data.Char (toUpper, toLower, isSpace)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
 | isSilence = "Fine. Be that way!"
 | isForcefulQuestion = "Calm down, I know what I'm doing!"
 | isQuestion = "Sure."
 | isShouting = "Whoa, chill out!"
 | otherwise = "Whatever."
    where
        isSilence = trimmedText == []
        isForcefulQuestion = isQuestion && isShouting
        isQuestion = last trimmedText == '?'
        isShouting = isUpperCaseEqual && isLowerCaseNotEqual
        trimmedText = dropWhileEnd isSpace xs
        isUpperCaseEqual = map toUpper trimmedText == trimmedText
        isLowerCaseNotEqual = map toLower trimmedText /= trimmedText