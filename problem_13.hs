-- Project Euler Problem 13
-- Find the first ten digits of the sum of one-hundred 50-digit numbers.

import System.IO

main = do
  withFile "inputs/problem_13.txt" ReadMode $ \handle -> do
         integers <- fmap formatting $ hGetContents handle
         print $ solve integers

formatting :: String -> [Integer]
formatting s = map read $ lines s

solve :: [Integer] -> String
solve ns = take 10 . show $ sum ns
