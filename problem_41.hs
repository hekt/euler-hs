{-  
    Problem 41
    What is the largest n-digit pandigital prime that exists?
    
    http://projecteuler.net/problem=41
 -}

import Data.List (permutations)
import MyMath (isPrime)
import MyList (list2int)

main = print solve

solve :: Int
solve = maximum . map list2int $ longestPPs 9
    where isPrime' ns
              | elem (last ns) [2,4,5,6,8] = False
              | otherwise                  = isPrime $ list2int ns
          longestPPs 0 = []
          longestPPs n
              | null nss  = longestPPs (n-1)
              | otherwise = nss
              where nss = filter isPrime' $ permutations [1..n]
