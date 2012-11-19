{- 
   Problem 14
   Find the longest sequence using a starting number under on million.
   
   http://projecteuler.net/problem=14
 -}

import Data.List (foldl1')
import Data.Ord (comparing)
import Data.Array

main = print $ solve (10^6-1)

solve :: Int -> Int
solve n = fst . maximumBy' (comparing snd) $ collatzLengths n

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' o xs = foldl1' (maxBy o) xs

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy o x y
    | o x y == GT = x
    | otherwise   = y

collatzLengths :: Int -> [(Int, Int)]
collatzLengths n = assocs ary
    where ary = listArray (1, n) [clzLen m m | m <- [1..n]]
          clzLen 1 _ = 1
          clzLen l m
              | l < m = (ary ! l)
              | even l    = 1 + clzLen (l `div` 2) m
              | otherwise = 1 + clzLen (l * 3 + 1) m
