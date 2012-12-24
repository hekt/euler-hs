{-
  Problem 66: Diophantine equation
  http://projecteuler.net/problem=66
 -}

import Data.Ord (comparing)
import Data.List (maximumBy)
import MyMath (isInt, isqrt)

main = print $ solve 1000

solve :: Integral a => a -> a
solve n = maximumBy (comparing pell) [d | d <- [1..n], not $ isSquare d]

isSquare :: Integral a => a -> Bool
isSquare = isInt . sqrt . fromIntegral

-- http://yasutech.blogspot.jp/2012/05/pell.html
pell  :: Integral a => a -> (a,a)
pell' :: Integral a => a -> (a,a,a,a,a) -> (a,a,a,a,a) -> (a,a)
pell d = let k = isqrt d
         in pell' d (0,0,0,0,1) (0,1,k,1,0)
pell' d (g, h, k, x, y) n@(gn, hn, kn, xn, yn)
    | g' == gn = let xr = xn^2 + d * yn ^ 2 `div` hn
                     yr = 2 * xn * yn `div` hn
                 in  (xr, yr)
    | h' == hn = let xr_ = (xn * x' + d * yn * y') `div` hn
                     yr_ = (xn * y' + x' * yn) `div` hn
                     xr  = xr_^2 + d * yr_^2
                     yr  = 2 * xr_ * yr_
                 in (xr, yr)
    | otherwise = pell' d n (g',h',k',x',y')
    where g' = (-gn) + kn * hn
          h' = (d - g'^2) `div` hn
          k' = (isqrt d + g') `div` h'
          y' = y + kn * yn
          x' = g' * y' + h' * yn
