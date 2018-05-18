{-
decodeMorse ".... . -.--   .--- ..- -.. ."
--should return "HEY JUDE"
-}

import Data.Map.Strict hiding (null)
import Debug.Trace

-- Utilities

trimWhile :: (Char -> Bool) -> String -> String
trimWhile p = trimF . trimF
  where trimF = reverse . dropWhile p

replace :: Char -> Char -> String -> String
replace new old xs = fmap (\x -> if x == old then new else x) xs

takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile _ [] = ([], [])
takeDropWhile p (x:xs) 
  | p x = (x:taken, dropped)
  | otherwise = ([], x:xs)
  where (taken, dropped) = takeDropWhile p xs

-- Morse to words

morseCodes :: Map String String
morseCodes = read "fromList [(\"-\",\"T\"),(\"--\",\"M\"),(\"---\",\"O\"),(\"-----\",\"0\"),(\"----.\",\"9\"),(\"---..\",\"8\"),(\"---...\",\":\"),(\"--.\",\"G\"),(\"--.-\",\"Q\"),(\"--..\",\"Z\"),(\"--..--\",\",\"),(\"--...\",\"7\"),(\"-.\",\"N\"),(\"-.-\",\"K\"),(\"-.--\",\"Y\"),(\"-.--.\",\"(\"),(\"-.--.-\",\")\"),(\"-.-.\",\"C\"),(\"-.-.--\",\"!\"),(\"-.-.-.\",\";\"),(\"-..\",\"D\"),(\"-..-\",\"X\"),(\"-..-.\",\"/\"),(\"-...\",\"B\"),(\"-...-\",\"=\"),(\"-....\",\"6\"),(\"-....-\",\"-\"),(\".\",\"E\"),(\".-\",\"A\"),(\".--\",\"W\"),(\".---\",\"J\"),(\".----\",\"1\"),(\".----.\",\"'\"),(\".--.\",\"P\"),(\".--.-.\",\"@\"),(\".-.\",\"R\"),(\".-.-.\",\"+\"),(\".-.-.-\",\".\"),(\".-..\",\"L\"),(\".-..-.\",\"\\\"\"),(\".-...\",\"&\"),(\"..\",\"I\"),(\"..-\",\"U\"),(\"..---\",\"2\"),(\"..--.-\",\"_\"),(\"..--..\",\"?\"),(\"..-.\",\"F\"),(\"...\",\"S\"),(\"...-\",\"V\"),(\"...--\",\"3\"),(\"...---...\",\"SOS\"),(\"...-..-\",\"$\"),(\"....\",\"H\"),(\"....-\",\"4\"),(\".....\",\"5\")]" :: Map String String

morseToWords :: String -> String
morseToWords [] = []
morseToWords (' ':' ': ' ':xs) = " " ++ (morseToWords xs)
morseToWords (' ':xs) = "" ++ (morseToWords xs)
morseToWords xs = (morseCodes ! word) ++ (morseToWords left)
  where
    (word, left) = takeDropWhile (/= ' ') xs
    
decodeMorse :: String -> String
decodeMorse = morseToWords . trimWhile (== ' ')

-- Bits to morse

itemToMorse :: Int -> String -> String
itemToMorse unit item
    | item == replicate (unit * 1) '1' = "."
    | item == replicate (unit * 3) '1' = "-"
    | item == replicate (unit * 1) '0' = ""
    | item == replicate (unit * 3) '0' = " "
    | item == replicate (unit * 7) '0' = "   "
    | otherwise = ""

bitsToMorse :: Int -> String -> String
bitsToMorse _ [] = []
bitsToMorse unit xs = (itemToMorse unit item) ++ (bitsToMorse unit left)
  where
    char = head xs
    (item, left) = takeDropWhile (== char) xs

decodeBits :: String -> String
decodeBits xs = bitsToMorse unit trimmed
  where
    trimmed = trimWhile (== '0') xs
    zeros = words $ replace ' ' '1' trimmed
    ones = words $ replace ' ' '0' trimmed
    unit = if null zeros then length trimmed
           else min (minimum $ fmap length zeros) (minimum $ fmap length ones)
