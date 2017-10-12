-- Haskell file to use for ghci for stuff on the fly
capital :: String -> String
capital "" = "Empty string, whoops!"
capital word@(x:_) = "The first letter of " ++ word ++ " is " ++ [x]
