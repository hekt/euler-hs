{-  
    Problem 38
    What is the largest 1 to 9 pandigital that can be formed by multiplying a fixed number by 1, 2, 3, ... ?
    
    http://projecteuler.net/problem=38
 -}

import Data.List (sort)
import MyList (int2list)

main = print solve

solve :: Int
solve = last $ filter isPandigital [1..9999]

isPandigital :: Int -> Bool
isPandigital n = isPandigital' $ scanl f (int2list n) [2..9]
    where f acc x = acc ++ int2list (n*x)

isPandigital' :: [[Int]] -> Bool
isPandigital' [] = False
isPandigital' (x:xs)
    | length x > 9     = False
    | length x /= 9    = isPandigital' xs
    | sort x == [1..9] = True
    | otherwise        = isPandigital' xs
