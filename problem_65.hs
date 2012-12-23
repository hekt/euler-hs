{-
  Problem 65: Convergents of e
  http://projecteuler.net/problem=65
 -}

import MyMath (sumOfDigits)

main = print $ solve 100

solve :: Int -> Int
solve n = sumOfDigits . fst $ convFrac (2, take (n-1) es)

es :: Integral a => [a]
es = f 2
    where f n = [1,n,1] ++ f (n+2)

convFrac :: Integral a => (a, [a]) -> (a, a)
convFrac (n, aas) = let (a: as) = reverse aas
                    in f (1, a) as
    where f (x,y) [] = (n * y + x, y)
          f (x,y) (z:zs) = let x' = y
                               y' = x + y * z
                           in f (x', y') zs
