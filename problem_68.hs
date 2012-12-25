{-
  Promblem 68: Magic 5-gon ring
  http://projecteuler.net/problem=68
 -}

import Data.List (permutations)
import MyList (pop)

type Ring = [(Int, Int, Int)]

main = print solve

solve :: Integer
solve = maximum [ring2int ring | ns <- nss, check ns,
                 let ring = list2ring ns, isMagicRing ring]
    where nss = concatMap (gen [1..10]) [0..5]
          gen ys x = let (n,ns) = pop x ys
                     in map (n:) $ permutations ns
          check (n:ns) = let ns' = map (ns !!) [2,4,6,8]
                         in all (n<) ns' && any (==10) ns'

isMagicRing :: Ring -> Bool
isMagicRing (t:ts) = all ((==sum3 t) . sum3) ts
    where 
      sum3 (a,b,c) = a+b+c

list2ring  :: [Int] -> Ring
list2ring (a:b:cs) = f b (a:cs)
    where f n (x:[])   = (x,n,b): []
          f n (x:y:zs) = (x,n,y): f y zs

ring2int  :: (Integral a, Read a) => Ring -> a
ring2int ring = read $ f ring
    where f []           = []
          f ((a,b,c):ds) = concatMap show [a,b,c] ++ f ds
