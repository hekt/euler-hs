{- 
   Problem 21
   Evaluate the sum of all amicable pairs under 10000.
   
   http://projecteuler.net/problem=21
 -}

import MyMath (isqrt)
import Data.Array

main = print $ solve 9999

solve :: Int -> Int
solve n = sum $ amicables n

sumOfDivs' :: Int -> Int
sumOfDivs' n = 1 + sum [ x | x <- [2 .. (n `div` 2)]
                       , n `mod` x == 0]

amicables :: Int -> [Int]
amicables n = [ x | x <- [1..n] , let x' = ary ! x
              , x /= x', n >= x' , x == (ary ! x') ]
    where ary = listArray (1, n) [sumOfDivs' x | x <- [1..n]]
