module Bob (responseFor) where

import Data.Char(isSpace)

responseFor :: String -> String
responseFor xs
    | q && c     = "Calm down, I know what I'm doing!"
    | q          = "Sure."
    | c          = "Whoa, chill out!"
    | trimmed == ""  = "Fine. Be that way!"
    | otherwise  = "Whatever."
    where
        trimmed = strip xs
        q = endsWith trimmed '?'
        c = allCaps (filter (`elem` ['a'..'z'] ++ ['A'..'Z']) trimmed)

strip :: String -> String
strip xs = (reverse.lstrip.reverse.lstrip) xs

lstrip :: String -> String
lstrip "" = ""
lstrip str@(c:xs)
    | isSpace c = lstrip xs
    | otherwise = str

endsWith :: String -> Char -> Bool
endsWith [] _ = False
endsWith (s:[]) c = c == s
endsWith (_:ss) c = endsWith ss c

allCaps :: String -> Bool
allCaps []     = False
allCaps xs = all (`elem` ['A'..'Z']) xs