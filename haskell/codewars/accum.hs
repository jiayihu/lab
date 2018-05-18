{-
accum "abcd"    -- "A-Bb-Ccc-Dddd"
accum "RqaEzty" -- "R-Qq-Aaa-Eeee-Zzzzz-Tttttt-Yyyyyyy"
accum "cwAt"    -- "C-Ww-Aaa-Tttt"
-}

import Data.Char
import Data.List

accum :: [Char] -> [Char]
accum [] = []
accum xs = concat $ intersperse "-" [toUpper x : (replicate i (toLower x)) | (x, i) <- zip xs [0..]]
