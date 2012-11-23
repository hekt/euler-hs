module MyList
    ( pop
    , maximumBy'
    , maxBy
    ) where

import Data.List

pop :: Int -> [a] -> (a, [a])
pop n xs = (xs !! n, take n xs ++ drop (n+1) xs)

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' o xs = foldl1' (maxBy o) xs

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy o x y
    | o x y == GT = x
    | otherwise   = y
