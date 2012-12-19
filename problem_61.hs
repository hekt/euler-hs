{-
  Problem 61: Cyclical figurate numbers
  http://projecteuler.net/problem=61
 -}

import Data.List (permutations)
import MyList (list2int, int2list)

main = print $ sum solve

solve :: [Int]
solve = head [ reverse $ head ns | p <- ps, let ns = f p, ns /= [] ]
    where
      f (xs:xss) = filter isCycle $
                   foldl (\acc x -> cyclicals acc x) (map (:[]) xs) xss
      ps = map (fourDigits octagonals:) . permutations $ 
           map fourDigits [ triangles, squares, pentagonals
                          , hexagonals, heptagonals ]

cyclicals :: [[Int]] -> [Int] -> [[Int]]
cyclicals xs ys = concatMap f xs
    where f ns = map (:ns) $ filter (isCyclic $ head ns) ys

isCyclic :: Int -> Int -> Bool
isCyclic x y = (drop 2 $ int2list x) == (take 2 $ int2list y)

isCycle :: [Int] -> Bool
isCycle ns = isCyclic (head ns) (last ns)

fourDigits :: [Int] -> [Int]
fourDigits ns = takeWhile (<10^4) $ dropWhile (<10^3) ns

triangles   :: [Int]
squares     :: [Int]
pentagonals :: [Int]
hexagonals  :: [Int]
heptagonals :: [Int]
octagonals  :: [Int]
triangles   = [ n * (n + 1)     `div` 2 | n <- [0..] ]
squares     = [ n ^ 2                   | n <- [0..] ]
pentagonals = [ n * (3 * n - 1) `div` 2 | n <- [0..] ]
hexagonals  = [ n * (2 * n - 1)         | n <- [0..] ]
heptagonals = [ n * (5 * n - 3) `div` 2 | n <- [0..] ]
octagonals  = [ n * (3 * n - 2)         | n <- [0..] ]
