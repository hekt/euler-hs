{-
  Problem 52
  Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits in some order.
  
  http://projecteuler.net/problem=52
 -}

import MyList (int2list, list2int, perms)

main = print solve2

solve1 :: Int
solve1 = head [n | n <- [123456..], sieve n, check n]
    where
      sieve n = n <= (10 ^ digits n) `div` 6
      check n = all (flip elemAll $ int2list n) $ map (int2list . (*n)) [2..6]

solve2 :: Int
solve2 = head $ concatMap (\x -> [list2int n | n <- ps x, check n]) [6..]
    where 
      ps n = map (1:) $ perms (0:[2..9]) (n-1)
      check n' = all (flip elemAll n') $ 
                 map (int2list . (* list2int n')) [2..6]

elemAll :: (Eq a) => [a] -> [a] -> Bool
elemAll xs ys = all (flip elem ys) xs

digits :: Integral a => a -> Int
digits = length . int2list
