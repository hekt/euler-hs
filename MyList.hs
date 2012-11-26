module MyList
    ( pop
    , maximumBy'
    , maxBy
    , int2list
    , int2list'
    , list2int
    , combs
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

int2list :: Integral a => a -> [a]
int2list n = reverse $ int2list' n

int2list' :: Integral a => a -> [a]
int2list' n
    | n < 10 = n : []
    | otherwise = n `mod` 10 : int2list' (n `div` 10)

list2int :: Integral a => [a] -> a
list2int [] = 0
list2int (n:ns) = n * 10 ^ length ns + list2int ns

combs :: [a] -> Int -> [[a]]
combs [] _ = []
combs xs 1 = map (:[]) xs
combs (x:xs) n = [ x:y | y <- combs xs (n-1) ] ++ combs xs n
