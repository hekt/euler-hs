{-
  Problem 55
  How many lychrel numbers are there below ten-thousand?
  
  http://projecteuler.net/problem=55
 -}

import MyList (int2list, list2int, isPalindrome)

main = print solve

solve :: Int
solve = length $ filter isLychrel [1..9999]

isLychrel :: Integral a => a -> Bool
isLychrel n = isLychrel' n 49

isLychrel' :: Integral a => a -> a -> Bool
isLychrel' _ 0 = True
isLychrel' n c
    | isPalindrome $ int2list n' = False
    | otherwise                  = isLychrel' n' (c-1)
      where n' = n + (reverseNum n)

reverseNum :: Integral a => a -> a
reverseNum n = list2int . reverse $ int2list n
