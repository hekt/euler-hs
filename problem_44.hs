{-  
    Problem 44
    Find the smallest pair of pentagonal numbers whose sum and difference is pentagonal.
    
    http://projecteuler.net/problem=44
 -}

import Data.Maybe
import MyMath (isInt)

main = print $ solve'

solve = f (pentDiff jk) [fst jk ..]
    where
      jk = fromJust . head . filter isJust $ map pentPair [1..]
      f c (x:xs)
          | c <= pentDiff (x-1, x) = c
          | isNothing pair         = f c xs
          | pairDiff < c           = f pairDiff xs
          | otherwise              = f c xs
          where pair = pentPair x
                pairDiff = pentDiff $ fromJust pair

solve' :: Integral a => a
solve' = pentDiff . fromJust . head . filter isJust $ map pentPair [1..]

pentDiff :: Integral a => (a, a) -> a
pentDiff (j, k) = pentagonal k - pentagonal j

isPentagonal :: Integral a => a -> Bool
isPentagonal n = isInt $ (1 + sqrt (1 + 24 * n')) / 6
    where n' = fromIntegral n

pentagonal :: Integral a => a -> a
pentagonal n = n * (3 * n - 1) `div` 2

pentPair :: Integral a => a -> Maybe (a, a)
pentPair n = f [n-1, n-2 .. 1]
    where 
      s j k = pentagonal j + pentagonal k
      d j k = pentagonal k - pentagonal j
      f [] = Nothing
      f (p:ps)
          | not . isPentagonal $ s p n = f ps
          | not . isPentagonal $ d p n = f ps
          | otherwise                  = Just (p, n)