{-
  Problem 59
  Using a brute force attack, can you dctypt the cipher using XOR encryption?
  
  http://projecteuler.net/problem=59
 -}

import System.IO
import Data.Char (ord, chr)
import Data.Bits (xor)
import Data.List (sort)
import MyList (perms)

main = do
  withFile "inputs/problem_59.txt" ReadMode $ \handle -> do
         cipher <- fmap formatting $ hGetContents handle
         print $ solve cipher

formatting :: String -> [Int]
formatting s = read ("["++s++"]")

solve :: [Int] -> Int
solve s = sum $ brute s

brute :: [Int] -> [Int]
brute s = head . filter f $ map (code s) keys
    where keys = perms [ord 'a' .. ord 'z'] 3
          f x = all (flip elem (words $ map chr x)) ["the", "of", "and"]

code :: [Int] -> [Int] -> [Int]
code xs ks = zipWith xor xs (cycle ks)

count :: [Int] -> [(Int, Int)]
count ns = count' $ sort ns

count' :: [Int] -> [(Int, Int)]
count' [] = []
count' ns = (len, head ns): count' (drop len ns)
    where len = length $ takeWhile (==head ns) ns
