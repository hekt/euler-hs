-- Project Euler Problem 12
-- What is the first triangular number that has more than 501 divisors.

main = print $ sieve 501

sieve :: Int -> Int
sieve n = head . filter ((>=n) . factorCount) $ map nthTri [1..]

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

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral
