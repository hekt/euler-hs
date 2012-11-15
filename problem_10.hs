main :: IO()
main = print . sum $ takeWhile (<= 200000) primes

primes :: [Int]
primes = 2: filter f [3,5..]
    where f n = all (\x -> n `mod` x /= 0) (takeWhile (\x -> x^2 <= n) primes)
