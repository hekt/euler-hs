{- 
   Problem 24
   What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9?
   
   http://projecteuler.net/problem=24
 -}

import MyMath (fact)
import MyList (pop)
import Data.List (sort)

main = print . concat . map show $ solve (10^6) [0..9]

solve :: Int -> [Int] -> [Int]
solve n xs = nthPerm n $ sort xs

nthPerm :: Int -> [a] -> [a]
nthPerm _ [] = []
nthPerm n xs 
    | r == 0    = y : reverse ys
    | otherwise = z : nthPerm r zs
    where (q, r) = divMod n $ fact (length xs - 1)
          (y, ys) = pop q xs
          (z, zs) = pop (q+1) xs
