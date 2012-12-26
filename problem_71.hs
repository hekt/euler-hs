{-
  Problem 71: Ordered fractions
  http://projecteuler.net/problem=71
 -}

import Data.List (maximumBy)
import Data.Ord (comparing)

main = print . fst $ solve (10^6)

solve :: Integral a => a -> (a, a)
solve lim = let ds = if lim < 7 then [1..lim]
                     else [lim-6..lim]
            in maximumBy (comparing frac2dec) $ 
                   [(n',d) | d <- ds, let n = fromIntegral d / 7 * 3, n /= 3,
                    let n' = floor n, gcd n' d == 1]

frac2dec :: (Integral a, Fractional b) => (a, a) -> b
frac2dec (n, d) = fromIntegral n / fromIntegral d
