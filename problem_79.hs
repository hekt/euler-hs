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
search nss = f . nub $ map ((:[]) . head) nss
    where 
      f []               = Nothing
      f (xxs@(x:_) : qs) = let nexts = findNexts x nss
                               xxs'  = reverse xxs
                           in if null nexts && check xxs' nss
                              then Just xxs'
                              else f $ qs ++ map (:xxs) nexts

findNexts :: Eq a => a -> [[a]] -> [a]
findNexts x yss = nub $ concatMap f yss
    where f ys = case elemIndex x ys of 
                   Nothing -> []
                   Just n  -> drop (n+1) ys



check :: [Int] -> [[Int]] -> Bool
check ns mss = all f mss
    where f (_:[]) = True
          f (x:y:zs) = case (elemIndex x ns, elemIndex y ns) of 
                         (Nothing, _) -> False
                         (_, Nothing) -> False
                         (Just x', Just y') | x' > y'   -> False
                                            | otherwise -> f (y:zs)
