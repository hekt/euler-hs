{- 
   Problem 26
   Find the value of d < 1000 for which 1/d contains the longest recurring cycle
   
   http://projecteuler.net/problem=26
 -}

import Data.List (maximumBy, inits, tails)
import Data.Ord (comparing)

main = print $ solve 999

solve :: Int -> Int
solve n = maximumBy (comparing c) [1..n]
    where
      c = length . recurring . filter (flip elem ['0'..'9']) . 
          show . (1 /) . fromIntegral

recurring :: String -> String
recurring s = recurring' s ""

recurring' :: String -> String -> String
recurring' ccs@(_:cs) tmp
    | length ccs `div` 2 <= length tmp = tmp
    | length (f its) > length tmp      = recurring' cs (f its)
    | otherwise                        = recurring' cs tmp
    where its = tail $ zip (inits ccs) (tails ccs)
          f [] = ""
          f ((x,y):xys)
              | x == take (length x) y = x
              | otherwise              = f xys
