{- 
   Problem 23
   Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
   
   http://projecteuler.net/problem=23
 -}

import MyMath (sumOfDivs)
import Data.Array

solve :: Int
solve = sum [x | x <- [1..28123], (not $ isTwoAbundant x)]

isAbundant :: Int -> Bool
isAbundant n = n < sumOfDivs n

isTwoAbundant :: Int -> Bool
isTwoAbundant n = isTwoAbundant' n abundants'
    where abundants' = takeWhile (<= n `div` 2) abundants

isTwoAbundant' :: Int -> [Int] -> Bool
isTwoAbundant' _ [] = False
isTwoAbundant' n (m:ms)
    | abundantsAry ! (n - m) = True
    | otherwise              = isTwoAbundant' n ms

abundants :: [Int]
abundants = filter (abundantsAry !) [1..28123]

abundantsAry :: Array Int Bool
abundantsAry = listArray (1, 28123) [isAbundant n | n <- [1..28123]]
