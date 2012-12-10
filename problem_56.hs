{-
  Problem 56
  Considering natural numbers of the form, a^b, finding the maximum digital sum.
  
  http://projecteuler.net/problem=56
 -}

import MyMath (sumOfDigits)

main = print solve

solve :: Int
solve = maximum [sumOfDigits (a^b) | a <- [1..99], b <- [1..99]]
