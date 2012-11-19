{- 
   Problem 15
   Starting in the top left corner in a 20 by 20 gtid, how many routes are 
   there to the bottomright corner?
   
   http://projecteuler.net/problem=15
 -}

import MyMath (fact)

main = print $ solve (20, 20)

solve :: Integral a => (a, a) -> a
solve (x, y) = div (fact (x+y)) (fact x * fact y)
