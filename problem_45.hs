{-  
    Problem 45
    After 40755, what is the next triangle number that is also pentagonal and hexagonal?
    
    http://projecteuler.net/problem=45
 -}

import MyMath (isInt)

main = print solve

solve :: Integral a => a
solve = head $ filter isTP [hexagonal n | n <- [144..]]
    where isTP n = isPentagonal n && isTriangle n

hexagonal :: Integral a => a -> a
hexagonal n = n * (2 * n - 1)

isPentagonal :: Integral a => a -> Bool
isPentagonal n = isInt $ (1 + sqrt (1 + 24 * n')) / 6
    where n' = fromIntegral n

isTriangle :: Integral a => a -> Bool
isTriangle n = isInt $ (1 + sqrt (1 + 8 * n')) / 2
    where n' = fromIntegral n
