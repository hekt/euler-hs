{-
 Problem 75: Singular integer right triangles
 http://projecteuler.net/problem=75
 -}

import Data.List (foldl')
import qualified Data.Map as M

import MyMath (isqrt)
import MyList (sum')

main = print $ solve (15*10^5)

solve :: Integral a => a -> a
solve lim = let mlim = isqrt $ lim `div` 2
            in f [2..mlim] [1..] M.empty
    where f [] _ d = sum' . filter (==1) $ M.elems d
          f mms@(m:ms) (n:ns) d
              | m <= n       = f ms [1..] d
              | even (m+n)   = f mms ns d
              | gcd m n /= 1 = f mms ns d
              | otherwise    = let d' = foldl' g d [p, p+p .. lim]
                               in f mms ns d'
              where p = (2 * m) * (m+n)
          g acc x = if M.member x acc then M.adjust (1+) x acc
                    else M.insert x 1 acc
