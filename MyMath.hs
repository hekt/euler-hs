module MyMath
    ( isqrt
    , primes
    , isPrime
    ) where

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

primes :: [Int]
primes = filter isPrime [0..]

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = ip (2: [3, 5 .. (isqrt n)])
    where ip [] = True
          ip (x:xs)
              | n `mod` x == 0 = False
              | otherwise = ip xs
