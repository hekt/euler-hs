{-
  Problem 76: Counting summations
  http://projecteuler.net/problem=76
 -}

main = print $ solve 100

solve :: Integral a => Int -> a
solve n = foldl f (1 : repeat 0) ([1..n-1]) !! n
    where 
      f acc x = let (a, cc) = splitAt x acc
                in a ++ gen a cc
      gen _      []     = []
      gen (m:ms) (n:ns) = let mn = m + n in mn : gen (ms++[mn]) ns
