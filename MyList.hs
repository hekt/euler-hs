module MyList
    ( pop
    ) where

pop :: Int -> [a] -> (a, [a])
pop n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)
