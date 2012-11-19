{- 
   Problem 10
   Calculate the sum of all the primes below two million.
   
   http://projecteuler.net/problem=10
 -}

main :: IO()
main = print . sum $ takeWhile (<= 200000) primes

primes :: [Int]
primes = 2: filter f [3,5..]
    where f n = all (\x -> n `mod` x /= 0) (takeWhile (\x -> x^2 <= n) primes)
