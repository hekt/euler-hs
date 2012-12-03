{-
  Problem 50
  Which prime, below one-million, canbe written as the sum of the most consecutive primes?
  
  http://projecteuler.net/problem=50
 -}

import MyMath (isPrime, primes)

main = print $ solve (10^6)

solve :: Int -> Int
solve n = head . filter isPrime $ map (sum . f 0 [] . flip drop primes) [0..]
    where f c cs (p:ps)
              | p + c < n = f (c+p) (p:cs) ps
              | otherwise = cs
