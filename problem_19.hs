{- 
   Problem 19
   How many sundays fell on the first of the month during the twentieth 
   century?
   
   http://projecteuler.net/problem=19
 -}

main = print $ solve 100

solve :: Int -> Int
solve n = f (take (n+1) years) 1 - f (take 1 years) 1
    where f [] c = 0
          f ([]:ms) c = f ms c
          f ((d:ds):ms) c
              | (c + d) `mod` 7 == 0 = 1 + f (ds:ms) (c + d)
              | otherwise                = f (ds:ms) (c + d)

months :: [Int]
months = [31,28,31,30,31,30,31,31,30,31,30,31]

months' :: [Int]
months' = [31,29,31,30,31,30,31,31,30,31,30,31]

years :: [[Int]]
years = f 1900
    where
      f n
          | n `mod` 100 == 0 && n `mod` 400 /= 0 = months  : f (n+1)
          | n `mod` 4 == 0                       = months' : f (n+1)
          | otherwise                            = months  : f (n+1)
