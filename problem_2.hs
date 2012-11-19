{- 
   # Problem 2
   By considering the terms in the Fibonacci sequence whose values do not 
   exceed four million, find the sum of the even-valued terms.

   http://projecteuler.net/problem=2
 -}

main :: IO()
main = print . sum $ fibos 400000 fibonacci
    where 
      fibs _ [] = []
      fibs m (n:ns)
          | n > m     = []
          | even n    = n : fibs m ns
          | otherwise = fibs m ns

fibonacci :: [Int]
fibonacci = 1: 2: f 1 2
    where f x y = (x+y) : f y (x+y)
