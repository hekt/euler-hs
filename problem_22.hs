{- 
   Problem 22
   What is the total of all the name scores in the file of first names?
   
   http://projecteuler.net/problem=22
 -}

import System.IO
import Data.Char (ord, chr)
import Data.List (sort)

main = do
  withFile "inputs/problem_22.txt" ReadMode $ \handle -> do
         s <- fmap formatting $ hGetContents handle
         print $ solve s

formatting :: String -> [String]
formatting s = read ("[" ++ s ++ "]")

solve :: [String] -> Int
solve s = sum . map (\(x,y) -> x * nameScore y) . zip [1..] $ sort s

char2int :: Char -> Int
char2int c = ord c - (ord 'A' - 1)

nameScore :: String -> Int
nameScore s = sum $ map char2int s
