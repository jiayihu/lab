{-
###Lyrics... Pyramids are amazing! Both in architectural and mathematical sense.
If you have a computer, you can mess with pyramids even if you are not in Egypt
at the time. For example, let's consider the following problem. Imagine that you
have a plane pyramid built of numbers, like this one here:

   /3/
  \7\ 4 
 2 \4\ 6 
8 5 \9\ 3
Here comes the task...

Let's say that the 'slide down' is a sum of consecutive numbers from the top to 
the bottom of the pyramid. As you can see, the longest 'slide down' is 
3 + 7 + 4 + 9 = 23

Your task is to write a function longestSlideDown (in ruby: longest_slide_down) 
that takes a pyramid representation as argument and returns its' longest 'slide down'.
For example:
longestSlideDown [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]] -- => 23
-}

import Debug.Trace

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (e - s + 1) $ drop s xs
  where
    s = if start < 0 then 0 else start
    e = if end > (length xs - 1) then length xs - 1 else end

longestSD :: [Int] -> [[Int]] -> Int
longestSD sums [] = maximum sums
longestSD sums (xs:xss) = longestSD nextSums xss
  where
    nextSums = [(+x) $ maximum $ slice (i-1) i sums |(x, i) <- zip xs [0..]]

longestSlideDown :: [[Int]] -> Int
longestSlideDown (xs:xss) = longestSD xs xss
