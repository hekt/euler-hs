module MyList
    ( pop
    , maximumBy'
    , maxBy
    , int2list
    , int2list'
    , list2int
    , combs, repCombs
    , perms, repPerms
    , isPalindrome
    , lasts
    ) where

import Data.List (permutations, foldl1')

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

perms :: [a] -> Int -> [[a]]
perms xs n = concatMap permutations $ combs xs n

repCombs :: [a] -> Int -> [[a]]
repCombs [] _ = []
repCombs xs 1 = map (:[]) xs
repCombs xxs@(x:xs) n = [ x:y | y <- repCombs xxs (n-1)] ++ repCombs xs n

repPerms :: [a] -> Int -> [[a]]
repPerms [] _ = []
repPerms xs 1 = map (:[]) xs
repPerms xs n = [ x:y | x <- xs, y <- repPerms xs (n-1)]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs
    | xs == reverse xs = True
    | otherwise        = False

lasts :: Int -> [a] -> [a]
lasts n xs = drop (length xs - n) xs
