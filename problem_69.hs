{-
  Problem 69: totient maximum
  http://projecteuler.net/problem=69
 -}

import MyMath (primes)

main = print $ solve (10^6)

solve :: Integral a => a -> a
solve n = f primes 1
    where f (p:ps) c = let c' = p * c
                       in if c' > n then c else f ps c'
