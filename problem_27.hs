{- 
   Problem 27
   Find a quadratic formula that produces the maximum number of primes for consecutive values of n.
   
   http://projecteuler.net/problem=27
 -}

import Data.List (maximumBy)
import Data.Ord (comparing)
import MyMath (isPrime, primes)

main = print . (\(a,b) -> a*b) $ solve 999

-- f(n) = n^2 + an + b
-- f(0) = b
-- f(1) = 1 + a + b
solve :: Int -> (Int, Int)
solve mx = maximumBy (comparing cmp) pairs
    where ary = [isPrime n | n <- [0..]]
          pairs = [(a, b) | a <- [1,3..mx], b <- (takeWhile (<= mx) primes), 
                   ary !! (1+a+b)]
          cmp (a,b) = length $ takeWhile (ary !!) [n^2 + a*n + b | n <- [0..]]
