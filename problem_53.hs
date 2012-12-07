{- 
   Problem 53
   How many values of C(n,r), for 1 ≦ n ≦ 100, exceed one-million?
   
   http://projecteuler.net/problem=53
 -}

import MyMath (fact)

main = print solve

solve :: Int
solve = sum [count n [m..n] | n <- [23..100]]
    where 
      count n rs = length $ filter (\r -> n `c` r > 10^6) rs
      m = head $ filter (\r -> 100 `c` r > 10^6) [1..]

c :: Integral a => a -> a -> a
c n r = fact n `div` (fact r * fact (n-r))
