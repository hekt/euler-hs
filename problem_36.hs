{- 
   Problem 36
   Find the sum of all numbers less than one million, which are palindromic in base 10 and base 2.
   
   http://projecteuler.net/problem=36
 -}

import MyList (int2list, isPalindrome)
import MyMath (int2bin)

main = print . sum . solve $ 10^6-1

solve :: Int -> [Int]
solve mx = [ n | n <- [1..mx], f n]
    where f n = (isPalindrome $ int2list n) && (isPalindrome $ int2bin n)
