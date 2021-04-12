module SumOfMultiples (sumOfMultiples) where

import Data.Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sumOfMultiplesHelp factors [1..(limit-1)] empty

sumOfMultiplesHelp :: [Integer] -> [Integer] -> Set Integer -> Integer
sumOfMultiplesHelp [] _ ver = sum (toList ver)
sumOfMultiplesHelp (0:fs) can ver = sumOfMultiplesHelp fs can ver
sumOfMultiplesHelp (f:fs) can ver = sumOfMultiplesHelp fs can (union ver (fromList (Prelude.filter ((==0).(`mod` f)) can)))
