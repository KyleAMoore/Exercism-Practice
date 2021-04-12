module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n < 1 = Nothing
    | otherwise = Just (colHelp n 0)
    where
        colHelp :: Integer -> Integer -> Integer
        colHelp 1 lev = lev
        colHelp val lev = colHelp (if mod val 2 == 0 then div val 2 else 3*val+1) (lev + 1)
