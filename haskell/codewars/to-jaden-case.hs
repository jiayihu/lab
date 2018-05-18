{-

Not Jaden-Cased: "How can mirrors be real if our eyes aren't real"
Jaden-Cased:     "How Can Mirrors Be Real If Our Eyes Aren't Real"

-}

import Data.Char

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

toJadenCase :: String -> String
toJadenCase input = unwords $ fmap capitalize $ words input
