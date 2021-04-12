module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = divBy year 4 && not (divBy year 100) || divBy year 400

divBy :: Integer -> Integer -> Bool
divBy n m = mod n m == 0