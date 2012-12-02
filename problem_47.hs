{-  
    Problem 47
    Find the first four consecutive integers to have four distinct primes factors.
    
    http://projecteuler.net/problem=47
 -}

main = print . head $ solve 4

solve :: Int -> [Int]
solve 1 = [1]
solve n = f [1..]
    where
      f xxs@(x:xs)
          | n /= length fn                = f $ drop n xxs
          | n /= length f1                = f xs
          | any ((/=n) . length) fs       = f $ drop (n-2) xxs
          | redundant $ concat (f1:fn:fs) = f xs
          | otherwise                     = take n xxs
          where 
            f1 = factorization x
            fn = factorization $ xxs !! (n-1)
            fs = map factorization $ take (n-2) xs

redundant :: (Eq a) => [a] -> Bool
redundant [] = False
redundant (x:xs)
    | elem x xs = True
    | otherwise = redundant xs

factorization :: Int -> [(Int, Int)]
factorization n = f $ factorization' n (2:[3,5..])
    where 
      f [] = []
      f xxs@(x:xs) = (x, y) : f (drop y xxs)
          where y = length $ takeWhile (==x) xxs

factorization' :: Int -> [Int] -> [Int]
factorization' 1 _ = []
factorization' n pps@(p:ps)
    | p * p > n = [n]
    | r == 0 = p: factorization' q pps
    | otherwise = factorization' n ps
    where (q, r) = divMod n p
