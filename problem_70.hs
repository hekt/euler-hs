{-
  Problem 70: Totient permutation
  http://projecteuler.net/problem=70
 -}

import Data.List (sort)
import MyMath (primes)
import MyList (int2list)

main = print $ solve (10^7)

solve :: Integral a => a -> a
solve limit = f pps ps 0 (10^7)
    where pps@(p:ps) = takeWhile (<5000) $ dropWhile (<2000) primes
          f (_:[]) _ n _        = n
          f (_:x:xs) [] n r = f (x:xs) xs n r
          f xxs@(x:xn:xs) (y:ys) n r
              | n' > limit                    = f (xn:xs) xs n r
              | isPermutation n' c' && r' < r = f xxs ys n' r'
              | otherwise                     = f xxs ys n r
              where n' = x * y
                    c' = (x - 1) * (y - 1)
                    r' = fromIntegral n' / fromIntegral c'

isPermutation :: Integral a => a -> a -> Bool
isPermutation x y = f x == f y
    where f = sort . int2list
