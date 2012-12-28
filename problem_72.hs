{-
  Problem 72: Counting fractions
  http://projecteuler.net/problem=72
 -}

import Data.Array
import Data.Maybe (isJust, fromJust)
import MyMath (isPrime, phi')
import MyList (sum')

main = print $ solve (10^6)

solve :: (Integral a, Ix a) => a -> a
solve limit = sum' $ map (ary !) [2..limit]
    where ary = listArray (1, limit) [totient n | n <- [1..limit]]
          totient p
              | isPrime p   = p - 1
              | isJust fact = let (a_, b_) = fromJust fact
                                  a        = fromIntegral a_
                                  b        = fromIntegral b_
                              in (ary ! a) * (ary ! b)
              | otherwise   = phi' p
              where fact = factor p
                    f (p,_)  = let p' = fromIntegral p
                               in 1 - 1 / p'

factor :: Integral a => a -> Maybe (a, a)
factor n = f n [2 .. n `div` 2]
    where f _ [] = Nothing
          f n (d:ds) = let (q, r) = n `divMod` d
                       in if r == 0 && gcd d q == 1 then Just (d,q)
                          else f n ds
