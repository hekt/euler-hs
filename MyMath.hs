module MyMath
    ( isqrt
    , fact
    , primes
    , isPrime
    , sumOfDigits
    , sumOfDivs
    ) where

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

fact :: Integral a => a -> a
fact 0 = 1
fact n = n * fact (n-1)

primes :: Integral a => [a]
primes = filter isPrime [0..]

isPrime :: Integral a => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = ip (2: [3, 5 .. (isqrt n)])
    where ip [] = True
          ip (x:xs)
              | n `mod` x == 0 = False
              | otherwise = ip xs

sumOfDigits :: (Integral c, Read c, Show a) => a -> c
sumOfDigits n = sum . map (read . (:[])) $ show n

sumOfDivs :: Int -> Int
sumOfDivs n = 1 + sum [ x' | x <- [2 .. (isqrt n)]
                      , let r = n `mod` x, r == 0, let m = n `div` x
                      , let x' = if x == m then x else x + m]