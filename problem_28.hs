{- 
   Problem 28
   What is the sum of both diagonals in a 1001 by 1001 spiral?
   
   http://projecteuler.net/problem=28
 -}

main = print $ solve (1001 `div` 2 + 1)

solve :: Int -> Int
solve n = sum . map sum $ take n spiralCorners

spiralCorners :: [[Int]]
spiralCorners = [1]: (f [3,5..1001] 1)
    where  f (x:xs) c = map g [1..4] : f xs (g 4)
               where g n = c + ((x-1) * n)
