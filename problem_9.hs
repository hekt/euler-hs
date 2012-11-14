main :: IO()
main = print . (\(x,y,z) -> x*y*z) $ head . filter isPythagoras $ triplesSumTo 1000

triplesSumTo :: Int -> [(Int, Int, Int)]
triplesSumTo n = [(a, b, n-(a+b)) | a <- [1..m], b <- [a+1..a+m], b < n-(a+b)]
    where m = n `div` 3 - 1

isPythagoras :: (Int, Int, Int) -> Bool
isPythagoras (a, b, c)
    | a^2 + b^2 == c^2 = True
    | otherwise        = False
