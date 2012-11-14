main :: IO()
main = print . last $ nPrimes 10001

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = ip (2:[3,5..sqrtN])
    where sqrtN = floor . sqrt $ fromIntegral n
          ip [] = True
          ip (x:xs)
              | n `mod` x == 0 = False
              | otherwise = ip xs

nPrimes :: Int -> [Int]
nPrimes n = take n $ filter isPrime (2:[3,5..])
