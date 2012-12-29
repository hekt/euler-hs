{-
 Problem 74: Digit factorial chains
 http://projecteuler.net/problem=74
 -}

import MyMath (fact)
import MyList (int2list, list2int, repCombs)
import Data.List (permutations, nub)

main = print solve

solve :: Int
solve = let nss = concatMap (repCombs [0..9]) [1..6]
        in sum [count ns | ns <- nss, f ns == 60]
    where
      count     = length . nub . filter ((/=0) . head) . permutations
      f         = length . fchainLoop . list2int . rotate
      rotate ns = let (xs, ys) = span (==0) ns in (ys ++ xs)

factSum :: Integral a => a -> a
factSum = sum . map fact . int2list

fchainLoop :: Integral a => a -> [a]
fchainLoop n = f n []
    where f n tmp = if elem n tmp then tmp
                    else f (factSum n) (n: tmp)
