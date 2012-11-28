{-  
    Problem 37
    Find the sum of all eleven primes that are both truncatable from left to right and right to left.
    
    http://projecteuler.net/problem=37
 -}

import Data.List (inits, tails)
import MyMath (primes, isPrime)
import MyList (int2list, list2int)

main = print $ sum solve

solve :: [Int]
solve = take 11 [ n | n <- primes', f $ int2list n ]
    where primes' = dropWhile (< 10) primes
          f ns = (all' . body $ inits ns) && (all' . body $ tails ns)
          all' = all ((ips !!) . list2int)
          ips = [isPrime n | n <- [0..]]

body :: [a] -> [a]
body = init . tail
