module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA "" = Right ""
toRNA (x:xs)
    | not (elem x "GCTA") = Left x
    | otherwise =
        case toRNA xs of
            Left tl -> Left tl
            Right tl -> Right (case x of
                                'A' -> 'U'
                                'C' -> 'G'
                                'G' -> 'C'
                                'T' -> 'A'
                                _ -> error "This should not be possible."
                                :tl)
