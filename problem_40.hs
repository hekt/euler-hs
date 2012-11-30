{-  
    Problem 40
    Finding the n^th digit of the fractional part of the irrational number.
    
    http://projecteuler.net/problem=40
 -}

import MyList (int2list)

main = print solve

solve :: Int
solve = product $ map (numbers !!) [1, 10, 10^2, 10^3, 10^4, 10^5, 10^6]

numbers :: [Int]
numbers = f 0
    where f n = int2list n ++ f (n+1)
