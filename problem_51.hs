{-
  Problem 51
  Find the smallest prime which, by changing the same part of the number, can from eight different primes.
  
  http://projecteuler.net/problem=51
 -}

import MyList (int2list, list2int, combs)
import MyMath (isPrime)

main = print solve

solve :: Int
solve = head [x | x <- primes, f x, g $ ps x]
    where primes = [n | n <- [56003,56005..], isPrime n]
          f x = any (\y -> above (==y) 3 $ int2list x) [0,1,2]
          g xs = any (above isPrime 8) xs
          ps n = map e $ combs [1.. digits n - 1] 3
              where e ds = filter ((==digits n) . digits) $
                           map (\x -> changeDigits n ds x) [0..9]

above :: (a -> Bool) -> Int -> [a] -> Bool
above f n xs = above' xs 0
    where above' [] c = c == n
          above' (x:xs) c
              | c == n    = True
              | f x       = above' xs (c+1)
              | otherwise = above' xs c

digits :: Integral a => a -> Int
digits = length . int2list

changeDigit :: Integral a => a -> a -> a -> a
changeDigit n d m = list2int $ changeDigit' (int2list n) d m
          
changeDigit' :: Integral a => [a] -> a -> a -> [a]
changeDigit' ns d m = f ns 1
    where f (x:xs) c
              | c == d    = m: xs
              | otherwise = x: f xs (c+1)

changeDigits :: Integral a => a -> [a] -> a -> a
changeDigits n ds m = list2int $ changeDigits' (int2list n) ds m

changeDigits' :: Integral a => [a] -> [a] -> a -> [a]
changeDigits' ns [] _ = ns
changeDigits' ns (d:ds) m = changeDigits' n' ds m
    where n' = changeDigit' ns d m
          f = (/=0) . head