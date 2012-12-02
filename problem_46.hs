{-  
    Problem 46
    What is the smallest odd composite that connot be written as the sum of a prime and twice a square?
    
    http://projecteuler.net/problem=46
 -}

import MyMath (primes, isPrime)

main = print solve

solve :: Int
solve = head [n | n <- [9,11..], not $ isPrime n, not $ isWritten n]

twiceSquares :: [Int]
twiceSquares  = [n^2*2 | n <- [1..]]

isTwiceSquare :: Int -> Bool
isTwiceSquare n = f twiceSquares
    where
      f (x:xs)
          | n > x     = f xs
          | n < x     = False
          | n == x    = True

isWritten :: Int -> Bool
isWritten n = f primes
    where
      f (p:ps)
          | n < p               = False
          | isTwiceSquare (n-p) = True
          | otherwise           = f ps
