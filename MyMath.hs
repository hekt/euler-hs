module MyMath
    ( isqrt
    , fact
    , primes
    , isPrime
    , sumOfDigits
    , sumOfDivs
    , reduction
    , int2bin, bin2int
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

sumOfDigits :: (Integral a, Integral c, Read c, Show a) => a -> c
sumOfDigits n = sum . map (read . (:[])) $ show n

sumOfDivs :: Int -> Int
sumOfDivs n = 1 + sum [ x' | x <- [2 .. (isqrt n)]
                      , let r = n `mod` x, r == 0, let m = n `div` x
                      , let x' = if x == m then x else x + m]

reduction :: Integral a => (a, a) -> (a, a)
reduction (n, d) = (n `div` g, d `div` g)
    where g = gcd n d

int2bin :: Integral a => a -> [a]
int2bin 0 = [0]
int2bin 1 = [1]
int2bin n = int2bin q ++ [r]
    where (q, r) = divMod n 2

bin2int :: Integral a => [a] -> a
bin2int [] = 0
bin2int (n:ns) = n * 2 ^ length ns + bin2int ns
