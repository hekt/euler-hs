{-  
    Problem 42
    How manya triangle words does the list of common English words contain?
    
    http://projecteuler.net/problem=42
 -}

import System.IO
import Data.Char (ord)

main = do
  withFile "inputs/problem_42.txt" ReadMode $ \handle -> do
         ws <- fmap formatting $ hGetContents handle
         print $ solve ws

formatting :: String -> [String]
formatting s = read $ "["++s++"]"

solve :: [String] -> Int
solve ws = length $ filter isTriWord ws

chr2int :: Char -> Int
chr2int c = ord c - (ord 'A' - 1)

str2int :: String -> Int
str2int s = sum $ map chr2int s

triangle :: Int -> Int
triangle n = floor $ (1 / 2) * n' * (n'+1)
    where n' = fromIntegral n

triangles :: [Int]
triangles = [triangle n | n <- [1..]]

isTriangle :: Int -> Bool
isTriangle n = f triangles
    where f (t:ts)
              | n == t = True
              | n < t  = False
              | otherwise = f ts

isTriWord :: String -> Bool
isTriWord s = isTriangle $ str2int s
