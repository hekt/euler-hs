{-  
    Problem 49
    Find arithmetic sequences, made of prime terms, whose four digits are permutations of each other.
    
    http://projecteuler.net/problem=49
 -}

import Data.List (sort)
import Data.Array (listArray, (!))
import MyMath (primes, isPrime)
import MyList (int2list, list2int)

main = print . list2int . concatMap int2list $ 
       (\(n, d) -> [n, n+d, n+d+d]) solve

solve :: (Int, Int)
solve = head . filter (/=(1487,3330)) $
        [(n, d) | n <- ps, d <- ds n, f [n, n+d, n+d+d]]
    where f ns = (all (pAry !) $ tail ns) && isPerms ns
          ds n = [10,20 .. (9999-n) `div` 2]
          ps = takeWhile (<10000) $ dropWhile (<1000) primes
          pAry = listArray (1000,9999) [isPrime n | n <- [1000..9999]]

isPerms :: [Int] -> Bool
isPerms (x:xs) = all ((==(f x)) . f) xs
    where f = sort . int2list
