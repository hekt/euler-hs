{-  
    Problem 43
    Find the sum of all pandigital numbers with an unusual sub-string divisibility property.
    
    http://projecteuler.net/problem=43
 -}

import Data.List (permutations, (\\))
import MyList (list2int, combs)

main = print $ sum solve

solve :: Integral a => [a]
solve = map (\xs -> list2int $ (head $ [0..9] \\ xs): xs) $
        foldl (\acc x -> f x acc) nss [3,5,7,11,13,17]
    where
      nss = [p | p <- perms [0..9] 3, last p `mod` 2 == 0]
      f n ns = [xsy | xs <- ns, y <- [0..9] \\ xs,
                let xsy = xs ++ [y], list2int (lasts 3 xsy) `mod` n == 0]

perms :: [a] -> Int -> [[a]]
perms xs n = concatMap permutations $ combs xs n

lasts :: Int -> [a] -> [a]
lasts n xs = drop (length xs - n) xs
