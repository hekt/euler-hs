main :: IO()
main = print $ foldl1 lcm' nums
       where nums = [ (20 `div` 2 + 1) .. 20 ]

gcd' :: Int -> Int -> Int
gcd' x y = f (max x y) (min x y)
    where f m 0 = m
          f m n = f n (m `mod` n)

lcm' :: Int -> Int -> Int
lcm' 0 _ = 0
lcm' _ 0 = 0
lcm' x y = x*y `div` (gcd' x y)