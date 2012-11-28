{- 
   Problem 35
   How many circular primes are there below one million?
   
   http://projecteuler.net/problem=35
 -}

import MyMath (isPrime, primes)
import MyList (list2int, int2list, repPerms)

main = print $ length solve2

solve1 :: [Int]
solve1 = 2: 5: [ x | x <- primes', let x' = int2list x, f x', g x' ]
    where primes' = takeWhile (< 10^6) primes
          f ns = all (==False) $ map (flip elem ns) [2,4,5,6,8]
          g ns = all isPrime . map list2int . tail $ rotates ns

solve2 :: [[Int]]
solve2 = [2]: [5]: filter f ps
    where ps = concatMap (repPerms [1,3,7,9]) [1..6]
          f ns = all isPrime . map list2int $ rotates ns

rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

rotates :: [a] -> [[a]]
rotates xs = scanl (\acc x -> x acc) xs $ replicate (length xs - 1) rotate
