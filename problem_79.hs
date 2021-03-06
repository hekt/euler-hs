{-
  Problem 79: Passcode derivation
  http://projecteuler.net/problem=79
 -}

import System.IO
import Data.List (elemIndex, nub)
import MyList (list2int)

main = do
  withFile "inputs/problem_79.txt" ReadMode $ \handle -> do
         nss <- fmap formatting $ hGetContents handle
         case search nss of
           Nothing -> print "Nothing"
           Just ns -> print $ list2int ns

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
    where f ys = maybe [] (\n -> drop (n+1) ys) $ elemIndex x ys

check :: [Int] -> [[Int]] -> Bool
check ns mss = all f mss
    where f (_:[])   = True
          f (x:y:zs) = case (elemIndex x ns, elemIndex y ns) of 
                         (Just x', Just y') | x' > y'   -> False
                                            | otherwise -> f (y:zs)
                         otherwise -> False
