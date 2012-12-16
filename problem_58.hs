{-
  Problem 58
  Investigate the number of primes that lie on the diagonals of the spiral grid.
  
  http://projecteuler.net/problem=58
 -}

import MyMath (isPrime)

main = print solve

solve :: Int
solve = f [3,5..] 0 1
    where 
      f (n:ns) p l = if (10*p') `div` l' == 0
                     then n
                     else f ns p' l'
          where p' = p + primeCorner n
                l' = l + 4

count :: (a -> Bool) -> [a] -> Int
count f [] = 0
count f (x:xs) = if f x then 1 + count f xs
                 else count f xs

primeCorner :: Integral a => a -> Int
primeCorner n = count isPrime $ map f [1..3]
    where f m = n^2 - (n-1) * m