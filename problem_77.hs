{-
  Problem 77: Prime summations
  http://projecteuler.net/problem=77
 -}

import MyMath (primes)

main = print $ solve (5000-1)

solve :: Int -> Int
solve n = f (1:repeat 0) primes 2
    where
      f xs (p:ps) c = let (x, s) = splitAt p xs
                          xs' = x ++ g x s
                      in if xs' !! c > n then c
                         else f xs' ps (c+1)
      g (m:ms) (n:ns) = let mn = m + n in mn : g (ms++[mn]) ns
