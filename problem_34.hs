{- 
   Problem 34
   Find the sum of all nubers which are equal to the sum of the factorial of their digits.
   
   http://projecteuler.net/problem=34
 -}

import MyList (int2list, repCombs)
import MyMath (fact)
import Data.List (sort)
import Data.Array

main = print $ sum solve

solve :: [Int]
solve = map sumOfFacts $ filter f cs
    where cs = concatMap (repCombs [0..9]) [2..7]
          f ns = ns == (sort . int2list $ sumOfFacts ns)
          sumOfFacts ns = sum $ map (fAry !) ns
          fAry = listArray (0, 9) $ map fact [0..9]
