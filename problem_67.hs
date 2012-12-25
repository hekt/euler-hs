{-
  Problem 67: Maximum path sum II
  http://projecteuler.net/problem=67
 -}

import System.IO
import Data.Array (listArray ,(!))
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M

data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Eq, Show)

main = do
  withFile "inputs/problem_67.txt" ReadMode $ \handle -> do
      nss <- fmap formatting $ hGetContents handle
      print $ solve nss

solve :: Integral a => [[a]] -> a
solve nss = maximum . search $ list2tree nss

formatting :: (Integral a, Read a) => String -> [[a]]
formatting str = map (map read . words) $ lines str

list2tree :: [[a]] -> Tree (Int, a)
list2tree xss = f 1 1
    where maximum_depth = length xss
          ary = let xss' = concat xss
                in listArray (1, length xss') xss'
          f i d = if d > maximum_depth then Empty
                  else let i' = i + d
                           d' = d + 1
                       in Node (i, ary ! i) (f i' d') (f (i'+1) d')

search  :: (Num a, Ord a, Ord k) => Tree (k,a) -> [a]
search' :: (Num a, Ord a, Ord k) => [(Tree (k,a), a)] -> M.Map k a -> [a]
search tree = search' [(tree, 0)] M.empty
search' [] _ = []
search' qqs@(((Node (i,n) l r), c): qs) d
    | isJust fp && fromJust fp >= c' = search' qs d
    | l == Empty                     = c': search' qs d
    | otherwise                      = let qqs' = (l, c'): (r, c'): qqs
                                       in search' qqs' (M.insert i c' d)
    where c' = c + n
          fp = M.lookup i d
