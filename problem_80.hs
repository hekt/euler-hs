{-
  Problem 80: Square root digital expansion
  http://projecteuler.net/problem=80
 -}

main = print $ solve 100 100

solve :: Integral a => a -> Int -> a
solve x y = sum [sum n' | n <- [2..x],
                 let n' = take y $ sqrtDigits n, length n' /= 1]

sqrtDigits :: Integral a => a -> [a]
sqrtDigits n = f n [1,3..] 0
    where  f n (x:xs) c
               | n < x     = let x' = (x-1) * 10 + 1
                             in c : f (n*100) [x', x'+2 ..] 0
               | n == x    = [c+1]
               | otherwise = f (n-x) xs (c+1)
