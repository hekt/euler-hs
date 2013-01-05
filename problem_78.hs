{-
  Problem 78: Coin pertitons
  http://projecteuler.net/problem=78
 -}

import Data.Array

main = print $ solve (10^6)

solve :: (Integral a, Ix a) => a -> a
solve m = head $ filter check [1..]
    where check n = (parts ! n) `mod` m == 0
          parts = listArray (0, m) (1: map part [1..m])
          part k = sum . zipWith ($) (cycle [id, id, negate, negate]) $
                   [parts ! (k-n) | n <- takeWhile (<=k) pentagonals]

pentagonals :: Integral a => [a]
pentagonals = let ps = scanl1 (+) [1,4..]
                  ns = scanl1 (+) [2,5..]
              in concat $ zipWith (\x y -> [x, y]) ps ns
