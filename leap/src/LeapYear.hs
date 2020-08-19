module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = isDividableByFour && (isNotDividableByHundred || isDividableByFourhundred)
    where
        isDividableByFour = rem year 4 == 0
        isDividableByFourhundred = rem year 400 == 0
        isNotDividableByHundred = rem year 100 /= 0
