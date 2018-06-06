reversedToDigits :: Int -> [Int]
reversedToDigits x
  | x <= 0 = []
  | x > 0 = (x `mod` 10) : reversedToDigits (x `div` 10)

toDigits :: Int -> [Int]
toDigits x = reverse (reversedToDigits x)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs)
  | length xs `mod` 2 == 0 = [x * 2, y] ++ doubleEveryOther xs
  | otherwise = [x, y * 2] ++ doubleEveryOther xs

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Int -> Bool
validate xs = sumDigits(doubleEveryOther (toDigits xs)) `mod` 10 == 0
