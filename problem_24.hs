{- 
   Problem 24
   What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9?
   
   http://projecteuler.net/problem=24
 -}

import MyMath (fact)
import Data.List (sort)

main = print . concat . map show $ solve (10^6) [0..9]

solve :: Int -> [Int] -> [Int]
solve n xs = nthComb n xs

pop :: Int -> [a] -> (a, [a])
pop n xs = ((xs !! n'), f 1 xs)
    where n' = n - 1
          f _ (_:[]) = []
          f c yys@(y:ys)
              | c == n    = tail yys
              | otherwise = y : f (c+1) ys

nthComb :: Ord a => Int -> [a] -> [a]
nthComb n xs = nthComb' n $ sort xs

nthComb' :: Int -> [a] -> [a]
nthComb' _ [] = []
nthComb' n xs 
    | r == 0         = y : reverse ys
    | otherwise      = z : nthComb' r zs
    where (q, r) = divMod n $ fact (length xs - 1)
          (y, ys) = pop q xs
          (z, zs) = pop (q+1) xs
