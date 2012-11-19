-- 18: Find the maximum sum travelling from the top of the triangle to the base
-- http://projecteuler.net/problem=18

import System.IO
import Data.Array
import Data.Maybe (fromJust)
import qualified Data.Map as M

data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Eq)

main = do
  withFile "inputs/problem_18.txt" ReadMode $ \handle -> do
      nss <- fmap formatting $ hGetContents handle
      print $ solve nss

solve :: (Integral a) => [[a]] -> a
solve nss = search $ list2tri nss

formatting :: String -> [[Int]]
formatting s = map (map read . words) $ lines s

list2tri :: [[a]] -> Tree (Int, a)
list2tri xss = f 1 1
    where depth = length xss
          xss' = concat xss
          ary = listArray (1, length xss') xss'
          f n d
              | d > depth = Empty
              | otherwise = Node (n, ary ! n) 
                             (f (n+d) (d+1)) (f (n+d+1) (d+1))

search :: (Num a, Ord a, Ord k) => Tree (k, a) -> a
search tree = maximum $ search' [(tree, 0)] M.empty

search' :: (Num a, Ord k, Ord a) => [(Tree (k, a), a)] -> M.Map k a -> [a]
search' [] _ = []
search' queue@( ((Node (i,n) left right), m) : qs) d
    | M.member i d && (fromJust $ M.lookup i d) >= nm
                    = search' qs d
    | left == Empty = nm : search' qs d
    | otherwise     = search' ((left,nm):(right,nm):queue) (M.insert i nm d)
    where nm = n+m
