main :: IO()
main = print $ squareTwoOfSum nums - sumOfSquareTwo nums
    where nums = [1..100]

squareTwoOfSum :: [Int] -> Int
squareTwoOfSum ns = (^2) $ sum ns

sumOfSquareTwo :: [Int] -> Int
sumOfSquareTwo ns = sum $ map (^2) ns