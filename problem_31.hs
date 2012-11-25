{- 
   Problem 31
   Investigating combinations of English currency denominations.
   
   http://projecteuler.net/problem=31
 -}

-- わかんなくてほぼコピペ
-- http://d.hatena.ne.jp/rst76/20080407/1207576616

main = print $ solve 200 [200,100,50,20,10,5,2,1]

solve :: Int -> [Int] -> Int
solve _ (1:[]) = 1
solve limit (x:xs) = sum [solve (limit - x * y) xs | y <- [0..limit `div` x]]
