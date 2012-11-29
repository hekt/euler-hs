{-  
    Problem 39
    If p is the perimeter of a right angle triangle, {a, b, c}, which value, for p ≦ 1000, has the most solutions?
    
    http://projecteuler.net/problem=39
 -}

import Data.List (maximumBy)
import Data.Ord (comparing)
import MyMath (isInt)

main = print $ solve 1000

solve :: Int -> Int
solve n = maximumBy (comparing $ length . rightTriangles) [2,4..n]

rightTriangles :: Integral a => a -> [(a,a,a)]
rightTriangles p = [ (floor a, floor b, floor c) | n <- [1 .. p `div` 3], 
                     let (a, b, c) = f n, isInt b , a <= b]
    where f n = (a, b, a+b)
              where a = fromIntegral n
                    b = fromIntegral (p^2 - 2*p*n) / fromIntegral (2*p - 2*n)
