{- 
   # Problem 1
   Add all the natural numbers bellow one housand that are multiples of 3 or 5
   
   http://projecteuler.net/problem=1
 -}

main :: IO()
main = print . sum $  multis [1..1000]

isMultiOf3or5 :: Int -> Bool
isMultiOf3or5 n
    | n `mod` 3 == 0 = True
    | n `mod` 5 == 0 = True
    | otherwise      = False

multis :: [Int] -> [Int]
multis [] = []
multis (n:ns)
    | isMultiOf3or5 n = n : multis ns
    | otherwise       = multis ns
