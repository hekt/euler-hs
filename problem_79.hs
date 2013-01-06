{-
  Problem 79: Passcode derivation
  http://projecteuler.net/problem=79
 -}

import System.IO
import Data.List (elemIndex, nub)
import Data.Maybe (isNothing, fromJust)
import MyList (list2int)

main = do
  withFile "inputs/problem_79.txt" ReadMode $ \handle -> do
         nss <- fmap formatting $ hGetContents handle
         let result = search nss
         if isNothing result then print "Nothing"
         else print . list2int $ fromJust result

formatting :: String -> [[Int]]
formatting str = map (map (read . (:[]))) $ lines str

search :: [[Int]] -> Maybe [Int]
search nss = let mss = map (:[]) . nub $ map head nss in f mss
    where 
      f []               = Nothing
      f (xxs@(x:_) : qs) = let nexts = findNexts x nss
                               xxs'  = reverse xxs
                           in if null nexts && check xxs' nss then Just xxs'
                              else f $ qs ++ map (:xxs) nexts

findNexts :: Eq a => a -> [[a]] -> [a]
findNexts x yss = nub $ f x yss
    where f _ []       = []
          f x (ys:yss) = let i = elemIndex x ys
                             i' = fromJust i + 1
                         in if isNothing i then f x yss
                            else drop i' ys ++ f x yss

check :: [Int] -> [[Int]] -> Bool
check ns mss = all f mss
    where f (_:[]) = True
          f (x:y:zs) | isNothing x' = False
                     | isNothing y' = False
                     | fromJust x' > fromJust y' = False
                     | otherwise                 = f (y:zs)
                     where x' = elemIndex x ns
                           y' = elemIndex y ns
