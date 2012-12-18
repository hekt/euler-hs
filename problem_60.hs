{-
  Problem 60: Prime pair sets
  http://projecteuler.net/problem=60
 -}

import Data.Maybe (fromJust, isJust)
import MyMath (isPrime, primes)
import MyList (combs, list2int, int2list)

main = print . sum $ solve 5

solve :: Int -> [Int]
solve n = head [ fromJust xs'++[x] | x <- primes, 
                 let xs = primePairSet x, let xs' = search $ combs xs n',
                 sieve xs, isJust xs' ]
    where n' = n - 1
          sieve xs = length xs >= n'
          search []     = Nothing
          search (x:xs) = if isPrimePairSet x then Just x
                          else search xs

primePairSet :: Integral a => a -> [a]
primePairSet n = f n $ takeWhile (<n) primes
    where f _ []     = []
          f x (y:ys) = if isPrimePair (x, y) then y : f x ys
                       else f x ys

isPrimePairSet :: Integral a => [a] -> Bool
isPrimePairSet ns = all isPrimePair $ pairs ns

isPrimePair :: Integral a => (a, a) -> Bool
isPrimePair (x, y) = (isPrime . list2int $ x' ++ y') &&
                     (isPrime . list2int $ y' ++ x')
    where x' = int2list x
          y' = int2list y

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs
