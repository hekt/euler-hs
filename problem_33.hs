{- 
   Problem 33
   Discover all the fractions with an unorthodox caccelling method.
   
   http://projecteuler.net/problem=33
 -}

import Data.List (intersect, delete)
import MyList (int2list, list2int)
import MyMath (reduction)

main = print solve

solve :: Int
solve = snd . reduction . foldl1 f $
        [ (n, d) | n <- [10..98], d <- [n..99], isCurious (n, d) ]
    where f (n1,d1) (n2,d2) = (n1*n2, d1*d2)

isCurious :: (Int, Int) -> Bool
isCurious (n, d)
    | n == d          = False
    | n `mod` 10 == 0 = False
    | d `mod` 10 == 0 = False
    | common == []    = False
    | otherwise       = n' / d' == fromIntegral n / fromIntegral d
    where common = intersect (int2list n) (int2list d)
          n' = fromIntegral . list2int . delete (head common) $ int2list n
          d' = fromIntegral . list2int . delete (head common) $ int2list d
