{- 
   Problem 20
   Find the sum of digits in 100!
   
   http://projecteuler.net/problem=20
 -}

import MyMath (fact, sumOfDigits)

main = print $ solve 100

solve :: Integer -> Integer
solve n = sumOfDigits $ fact n
