module MyList
    ( pop
    , maximumBy'
    , maxBy
    , int2list
    , int2list'
    ) where

import Data.List

pop :: Int -> [a] -> (a, [a])
pop n xs = (xs !! n, take n xs ++ drop (n+1) xs)

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ []    = error "MyList.maximumBy': empty list"
maximumBy' cmp xs = foldl1' (maxBy cmp) xs

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y
    | cmp x y == GT = x
    | otherwise     = y

int2list :: Int -> [Int]
int2list n = reverse $ int2list' n

int2list' :: Int -> [Int]
int2list' n
    | n < 10 = n : []
    | otherwise = n `mod` 10 : int2list' (n `div` 10)
