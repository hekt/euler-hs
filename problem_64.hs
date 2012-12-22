{-
  Problem 64: Odd period square roots
  http://projecteuler.net/problem=64
 -}

import MyMath (isInt)

main = print solve

solve :: Int
solve = length [n | n <- [2..10000], sieve n, check n]
    where sieve = not . isInt . sqrt . fromIntegral
          check = odd . length . snd . cfracs

cfracs :: Integral a => a -> (a, [a])
cfracs n = (tr, f 1 tr)
    where tr = truncate . sqrt $ fromIntegral n
          f x y = let x' = (n - y ^ 2) `div` x
                      y' = tr - (tr + y) `mod` x'
                      a  = (tr + y) `div` x'
                  in if x' == 1 && y' == tr
                     then a : []
                     else a : f x' y'
