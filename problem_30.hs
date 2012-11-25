{- 
   Problem 30
   Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
   
   http://projecteuler.net/problem=30
 -}

import Data.Array
import MyList (int2list')

main = print . sum $ solve 5

solve n = [x | x <- [2..limit], f x == x]
    where limit = 9 ^ n * (n+1)
          f m = sum . map (ary !) $ int2list' m
          ary = listArray (0,9) $ map (^n) [0..9]
