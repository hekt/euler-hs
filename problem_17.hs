{- 
   Problem 17
   How many latters would be needed to write all the numbers in word from 1 to 
   1000?
   
   http://projecteuler.net/problem=17
 -}

import Data.Maybe (fromJust)
import qualified Data.Map as M

main = print $ solve [1..1000]

solve :: [Int] -> Int
solve ns = sum $ map num2sumOfLets ns

num2sumOfLets :: Int -> Int
num2sumOfLets 0 = 0
num2sumOfLets n
    | n < 20    = fromJust $ M.lookup n d
    | n < 100   = fromJust (M.lookup (digHead n * 10) d) +
                  num2sumOfLets (digTail n)
    | n == 100  = length "OneHundred"
    | n < 1000  = fromJust (M.lookup (digHead n) d) + length "HundredAnd" + 
                  num2sumOfLets (digTail n)
    | n == 1000 = length "OneThousand"
    where d = lettersMap

digHead :: Int -> Int
digHead n = read . (:[]) . head $ show n

digTail :: Int -> Int
digTail n = read . tail $ show n

lettersMap :: M.Map Int Int
lettersMap = M.fromList (a++b)
    where a = zip [1..19] . map length $
              [ "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight"
              , "Nine", "Ten", "Eleven", "Twelve", "Thirteen" , "Fourteen"
              , "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen" ]
          b = zip [20,30..90] . map length $
              [ "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy"
              , "Eighty", "Ninety" ]
