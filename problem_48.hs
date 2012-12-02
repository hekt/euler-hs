{-  
    Problem 48
    Find the last ten digits of 1^1 + 2^2 .. + 1000^1000.
    
    http://projecteuler.net/problem=48
 -}

import MyList (int2list, list2int, lasts)

main = print $ solve 1000

solve :: Integer -> Integer
solve n = list2int . lasts 10 . int2list $ sum [m^m | m <- [1..n]]
