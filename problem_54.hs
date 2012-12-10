{-
  Problem 54
  How many hands did player one win in the game of poker?
  
  http://projecteuler.net/problem=54
 -}

import Data.List (sort, elemIndices, nub)
import Data.Maybe (isJust, fromJust)
import System.IO

data Suit = D | H | C | S deriving (Eq, Read)
type Value = Int
type Card = (Value, Suit)
type Hand = [Card]

main = do
  withFile "inputs/problem_54.txt" ReadMode $ \handle -> do
         xs <- fmap formatting $ hGetContents handle
         print $ solve xs

solve :: [(Hand, Hand)] -> Int
solve xs = length $ filter (\(a,b) -> isWin a b) xs

formatting :: String -> [(Hand, Hand)]
formatting str = map (words2hands . words) $ lines str
    where words2hands ws = (words2hand $ take 5 ws, words2hand $ drop 5 ws)

str2card :: String -> Card
str2card (v:s:[]) = (vRead v, read [s])

words2hand :: [String] -> Hand
words2hand ws = map str2card ws

vRead :: Char -> Int
vRead 'A' = 14
vRead 'T' = 10
vRead 'J' = 11
vRead 'Q' = 12
vRead 'K' = 13
vRead c   = read [c]

isWin :: Hand -> Hand -> Bool
isWin a b = a' > b'
    where a' = hand a
          b' = hand b

hand :: Hand -> (Int, [Value], [Value])
hand h
    | isJust $ royalFlush h    = (9, fromJust $ royalFlush h,    [])
    | isJust $ straightFlush h = (8, fromJust $ straightFlush h, [])
    | isJust $ fourOfAKind h   = (7, fromJust $ fourOfAKind h,   highVals h')
    | isJust $ fullHouse h     = (6, fromJust $ fullHouse h,     [])
    | isJust $ flush h         = (5, fromJust $ flush h,         [])
    | isJust $ straight h      = (4, fromJust $ straight h,      [])
    | isJust $ threeOfAKind h  = (3, fromJust $ threeOfAKind h,  highVals h')
    | isJust $ twoPairs h      = (2, fromJust $ twoPairs h,      highVals h')
    | isJust $ onePair h       = (1, fromJust $ onePair h,       highVals h')
    | otherwise                = (0, highCard h,                 [])
    where h' = values h

vSucc :: Value -> Value
vSucc 14 = 2
vSucc v = succ v

values :: Hand -> [Value]
values hand = map fst hand

suits :: Hand -> [Suit]
suits hand = map snd hand

sameValues :: [Value] -> [Int]
sameValues vs = map (length . flip elemIndices vs) vs

isSameSuit :: Hand -> Bool
isSameSuit ((_, s) : cs) = all (== s) $ suits cs

isConsecutive :: Hand -> Bool
isConsecutive hand = isConsecutive' . sort $ values hand

isConsecutive' :: [Value] -> Bool
isConsecutive' (_:[]) = True
isConsecutive' (x:y:zs)
    | vSucc x == y = isConsecutive' (y:zs)
    | otherwise    = False

highVals :: [Value] -> [Value]
highVals vs = reverse . sort $ nub vs

conVals :: [Value] -> [Value]
conVals vs = reverse . sort $ f vs
    where f [] = []
          f (x:xs)
              | elem x xs = x: f xs
              | otherwise = f xs

royalFlush :: Hand -> Maybe [Value]
royalFlush hand = if isSameSuit hand && (sort $ values hand) == [10,11,12,13,14]
                    then Just . highVals $ values hand
                    else Nothing

straightFlush :: Hand -> Maybe [Value]
straightFlush hand = if isSameSuit hand && isConsecutive hand
                     then Just . highVals $ values hand
                     else Nothing

fourOfAKind :: Hand -> Maybe [Value]
fourOfAKind hand = if elem 4 . sameValues $ values hand
                   then Just . conVals $ values hand
                   else Nothing

fullHouse :: Hand -> Maybe [Value]
fullHouse hand = if (elem 2 $ sameValues vs) && (elem 3 $ sameValues vs)
                 then Just . highVals $ values hand
                 else Nothing
    where vs = values hand

flush :: Hand -> Maybe [Value]
flush hand = if (isSameSuit hand)
             then Just . highVals $ values hand
             else Nothing

straight :: Hand -> Maybe [Value]
straight hand = if (isConsecutive hand)
                then Just . highVals $ values hand
                else Nothing

threeOfAKind :: Hand -> Maybe [Value]
threeOfAKind hand = if elem 3 . sameValues $ values hand
                      then Just . conVals $ values hand
                      else Nothing

twoPairs :: Hand -> Maybe [Value]
twoPairs hand = if (length . elemIndices 2 . sameValues $ values hand) == 4
                  then Just . conVals $ values hand
                  else Nothing

onePair :: Hand -> Maybe [Value]
onePair hand = if elem 2 . sameValues $ values hand
                 then Just . conVals $ values hand
                 else Nothing

highCard :: Hand -> [Value]
highCard hand = [maximum $ values hand]