{-
  Problem 77: Prime summations
  http://projecteuler.net/problem=77
 -}

import MyMath (primes)

main = print $ solve 5000

solve :: Int -> Int
solve n = f (1:repeat 0) primes
    where
      f xs (p:ps) = let (x, s) = splitAt p xs
                        xs'    = x ++ g x s
                    in if xs' !! p >= n then h (p-1) xs'
                       else f xs' ps
      g (m:ms) (n:ns) = let mn = m + n in mn : g (ms++[mn]) ns
      h i xs = if xs !! i >= n then h (i-1) xs
               else i+1
