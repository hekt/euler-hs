{-
  Problem 73: Counting fractions in a range
  http://projecteuler.net/problem=73
 -}

main = print $ solve 12000

solve :: Integral a => a -> a
solve lim = f 2 3
    where f a b = let ab = a + b
                  in if ab > lim then 0
                     else 1 + f a ab + f b ab
