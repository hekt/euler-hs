{-
  Problem 51
  Find the smallest prime which, by changing the same part of the number, can from eight different primes.
  
  http://projecteuler.net/problem=51
 -}

import MyList (int2list, list2int, combs)
import MyMath (isPrime)

main = print solve

solve :: Int
solve = head [x | x <- primes, sieve (cnt x) x]
    where 
      primes = [n | n <- [10001,10003..], isPrime n]
      sieve n x
          | n < 3     = False
          | n == 3    = check $ perms 3 x
          | otherwise = (check $ perms n x) || sieve (n-1) x
      cnt x = maximum $ map (\y -> count (==y) $ int2list x) [0,1,2]
      check xs = any (above isPrime 8) xs
      perms n x = map e $ combs [1 .. digx - 1] n
          where digx = digits x
                e ds = filter ((==digx) . digits) $
                       map (\y -> changeDigits x ds y) [0..9]

count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs)
    | f x = 1 + count f xs
    | otherwise = count f xs

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
