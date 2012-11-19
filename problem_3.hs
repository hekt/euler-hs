{- 
   # Problem 3
   Find the largest prime factor of a composite number.

   http://projecteuler.net/problem=3
 -}

main :: IO()
main = print . head $ primeFactors 600851475143

primeFactors :: Int -> [Int]
primeFactors n = f primes []
    where 
      primes = 2:[3,5..(floor . sqrt $ fromIntegral n)]
      f [] factor = factor
      f (x:xs) factor
          | isPrime x && n `mod` x == 0 = f xs (x:factor)
          | product factor == n         = factor
          | otherwise                   = f xs factor

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = f (2:[3,5..sqrtN])
    where sqrtN = floor . sqrt $ fromIntegral n
          f [] = True
          f (x:xs)
              | n `mod` x == 0 = False
              | otherwise = f xs
