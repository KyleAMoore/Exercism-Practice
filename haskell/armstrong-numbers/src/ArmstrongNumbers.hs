module ArmstrongNumbers (armstrong) where

armstrong :: Int -> Bool
armstrong n = let str = show n in n == sum [(read [c])  ^ (length str) | c <- str, elem c "1234567890"]
