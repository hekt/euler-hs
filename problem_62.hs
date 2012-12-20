{-
  Problem 62: Cubic permutations
  http://projecteuler.net/problem=62
 -}

import Data.List (sort)
import MyList (int2list)

main = print $ solve 5

solve :: Integral a => Int -> a
solve n = head [x | x <- cubes, f x == n]
    where 
      cubes' = map (sort . int2list) $ cubes
      sameDigitsCubes' x = takeWhile ((==d) . length) $
                           dropWhile ((/=d) . length) cubes'
          where d = length $ int2list x
      f x = length . filter (==x') $ sameDigitsCubes' x
          where x' = sort $ int2list x

cubes :: Integral a => [a]
cubes = [n^3 | n <- [0..]]
