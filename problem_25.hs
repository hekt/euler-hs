{- 
   Problem 25
   What is first tem in the Fibonacci sequence to contain 1000 digits?
   
   http://projecteuler.net/problem=25
 -}

main = print . fst $ solve (10^999)

solve :: Integer -> (Int, Integer)
solve n = head . filter ((>= n) . snd) $ zip [1..] fibs

fibs :: Integral a => [a]
fibs = 1 : 1 : fibs' 1 1

fibs' :: Integral a => a -> a -> [a]
fibs' x y = (x + y) : fibs' y (x+y)
