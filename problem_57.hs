{-
  Problem 57
  Invenstigate the expansion of the continued fraction for the square root of two.
  
  http://projecteuler.net/problem=57
 -}

import MyList (int2list)

main = print $ solve 1000

solve :: Int -> Int
solve n = length . filter f $ take n continuedRootTwo
    where f (a,b) = digits a > digits b

continuedRootTwo :: Integral a => [(a, a)]
continuedRootTwo = (3,2): f (1,2)
    where f (a,b) = (c+d, d): f (c,d)
              where (c,d) = (b, b*2+a)

digits :: Integral a => a -> Int
digits = length . int2list
