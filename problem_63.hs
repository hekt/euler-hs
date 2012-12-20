{-
  Problem 63: Powerful digit counts
  http://projecteuler.net/problem=63
 -}

import MyList (int2list)

main = print solve

solve :: Int
solve = length $ concatMap f [1..9]
    where 
      f x = takeWhile (isPowerfulDigit x) $ 
            dropWhile (not . isPowerfulDigit x) [1..]

isPowerfulDigit :: Integral a => a -> a -> Bool
isPowerfulDigit x n = dig == n
    where dig = fromIntegral . digits $ x^n

digits :: Integral a => a -> Int
digits n = length $ int2list n
