{- 
   Problem 23
   Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
   
   http://projecteuler.net/problem=23
 -}

import MyMath (sumOfDivs)
import Data.Array

main = print solve

solve :: Int
solve = sum [x | x <- [1..28123], (not $ isTwoAbundant x)]

isAbundant :: Int -> Bool
isAbundant n = n < sumOfDivs n

isTwoAbundant :: Int -> Bool
isTwoAbundant n = f abundants'
    where abundants' = takeWhile (<= n `div` 2) abundants
          f [] = False
          f (m:ms)
              | abundantsAry ! (n-m) = True
              | otherwise            = f ms

abundants :: [Int]
abundants = filter (abundantsAry !) [1..28123-12+1]

abundantsAry :: Array Int Bool
abundantsAry = listArray (1, 28123-12+1) $
               [isAbundant x | x <- [1..28123-12+1]]
