{- 
   Problem 12
   What is the value of the first triangle number to have over five hundred 
   divisors?
   
   http://projecteuler.net/problem=12
 -}

import MyMath

main = print $ solve 501

solve :: Int -> Int
solve n = head . filter ((>=n) . factorCount) $ map nthTri [1..]

nthTri :: Int -> Int
nthTri n = n * (n + 1) `div` 2

factorization :: Int -> [Int]
factorization n = f n primes
    where f 1 _ = []
          f m pps@(p:ps)
              | m `mod` p == 0 = p: f (m `div` p) pps
              | otherwise      = f m ps

factorCount :: Int -> Int
factorCount n = product . map ((+)1) . countWhile $ factorization n

countWhile :: Eq a => [a] -> [Int]
countWhile [] = []
countWhile xxs@(x:_) = (length $ takeWhile (==x) xxs) : 
                       (countWhile $ dropWhile (==x) xxs)
