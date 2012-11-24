{- 
   Problem 27
   Find a quadratic formula that produces the maximum number of primes for consecutive values of n.
   
   http://projecteuler.net/problem=27
 -}

import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)
import MyMath (isPrime, primes)

main = print . (\(a,b) -> a*b) $ solve 999

-- f(n) = n^2 + an + b
-- f(0) = b
-- f(1) = 1 + a + b
solve :: Int -> (Int, Int)
solve mx = maximumBy (comparing cmp) pairs
    where 
      ip n = if n <= (mx) then ary1 ! n else ary2 !! (n-mx)
      ary1 = listArray (0,mx) [isPrime n | n <- [0..mx]]
      ary2 = [isPrime n | n <- [mx+1..]]
      pairs = [(a, b) | a <- as, b <- bs,  (ip $ abs (1+a+b))]
          where as = [-999, -997 .. mx]
                bs = takeWhile (<= mx) primes ++ 
                     (map negate $ takeWhile (<= mx) primes)
      cmp (a,b) = length $ takeWhile (ip . abs) [n^2 + a*n + b | n <- [0..]]
