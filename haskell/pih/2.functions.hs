halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
  where half = (length xs) `div` 2

third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:z:_) = z

null' :: [a] -> Bool
null' [] = True
null' _ = False

safetail :: [a] -> [a]
safetail xs = if null' xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null' xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs


-- Luhn algorithm

luhnDouble :: Int -> Int
luhnDouble x
  | double > 9 = double - 9
  | otherwise = double
  where double = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z
  | total `mod` 10 == 0 = True
  | otherwise = False
  where total = sum [luhnDouble w, x, luhnDouble y, z]
