{- 
   Problem 11
   What is the greatest product of four adjacent number on the same straight 
   line in th 20 by 20 grid?
   
   http://projecteuler.net/problem=11
 -}

import System.IO
import Data.Maybe

main = do
  withFile "inputs/problem_11.txt" ReadMode $ \handle -> do
      table <- fmap formatting $ hGetContents handle
      print . maximum . concat $ map (\xy -> seqs xy 4 table) 
             [(x,y) | x <- [0..(length $ head table)-1], 
              y <- [0..(length table)-1]]

formatting :: String -> [[Int]]
formatting str = map (map read . words) $ lines str

seqFuncs :: [ ((Int,Int) -> Int -> [[a]] -> [a]) ]
seqFuncs = [r, b, tr, rb]
    where r (x,y) n table = map (\m -> table !! y !! (x+m)) [0..n-1]
          b (x,y) n table = map (\m -> table !! (y+m) !! x) [0..n-1]
          tr (x,y) n table = map (\m -> table !! (y-m) !! (x+m)) [0..n-1]
          rb (x,y) n table = map (\m -> table !! (y+m) !! (x+m)) [0..n-1]

seqConds :: [ ((Int, Int) -> Int -> [[a]] -> Bool) ]
seqConds = [r, b, tr, rb]
    where t (x,y) n table = 0 <= (y-n+1)
          r (x,y) n table = (x+n-1) < (length $ head table)
          b (x,y) n table = (y+n-1) < (length table)
          tr (x,y) n table = t (x,y) n table && r (x,y) n table
          rb (x,y) n table = r (x,y) n table && b (x,y) n table

seqs :: (Int, Int) -> Int -> [[Int]] -> [Int]
seqs (x,y) n table = map (product . fromJust) . filter (/= Nothing) $ map f s
    where s = zip seqConds seqFuncs
          f (a, b) = if a (x,y) n table == True then Just $ b (x,y) n table
                     else Nothing
