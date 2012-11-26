{- 
   Problem 32
   Find the sum of all numbers that can be written pandigital products.
   
   http://projecteuler.net/problem=32
 -}

import Data.List (sort, permutations, nub)
import MyList (int2list, list2int, combs)

main = print solve

solve :: Int
solve = sum . nub $ map (\(x,y) -> x*y) one2nines

one2nines :: [(Int, Int)]
one2nines = filter isOne2nine ns
    where ns = [ (x, y) | x <- [1..9], y <- combs' 4, x*y < 10000 ] ++ 
               [ (x, y) | x <- combs' 2, y <- combs' 3, x*y < 10000 ]
          combs' = map list2int . concat . map permutations . combs [1..9]

isOne2nine :: (Int, Int) -> Bool
isOne2nine (x,y) = xyz == [1..9]
    where xyz = (sort . concat $ map int2list [x,y,x*y])
