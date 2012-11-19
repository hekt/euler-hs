-- 16: What is the sum of the digits of the number 2^1000?
-- http://projecteuler.net/problem=16

main = print $ sumOfDigs (2^1000)

sumOfDigs :: Integer -> Integer
sumOfDigs n = sum . map (read . (:[])) $ show n
