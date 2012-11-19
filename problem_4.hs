{- 
   # Problem 4
   Find the largest palindrome made from the product of two 3-digit numbers.

   http://projecteuler.net/problem=4
 -}
main :: IO()
main = print $ maximumOfPalindNum' 100 999

isPalindrome :: [Char] -> Bool
isPalindrome xs
    | xs == reverse xs = True
    | otherwise        = False

isPalindNum :: Int -> Bool
isPalindNum n = isPalindrome $ show n

maximumOfPalindNum :: Int -> Int -> Int
maximumOfPalindNum mn mx = maximum [x*y | x <- [mn..mx], y <- [x..mx],
                                    isPalindNum (x*y)]

maximumOfPalindNum' :: Int -> Int -> Int
maximumOfPalindNum' mn mx = f [mx, mx-1 .. mn] [mx, mx-1 .. mn] 0
    where f [] _ z = z
          f (x:xs) [] z = f xs [x-1, x-2 .. mn] z
          f xxs@(x:xs) (y:ys) z
              | (x*y) < z         = f xs [x-1, x-2 .. mn] z
              | isPalindNum (x*y) = f xs [x-1, x-2 .. mn] (x*y)
              | otherwise         = f xxs ys z
