module Shared where

import Data.Char(chr,ord)
import Data.List(find,union)
import Maybe

fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--simple approach
triangleNo x = sum [1..x]
--recursive approach
triangleNo' 1 = 1
triangleNo' x = x + triangleNo' (x-1)

triangleNos = [triangleNo x | x <- [1,2..]]
triangleNos' = [triangleNo' x | x <- [1,2..]]

factorial x = if x > 0 then product [1..x] else 0

pentagonalNo x = x * (3 * x - 1) `div` 2

hexagonalNo x = x * (2 * x - 1)

allTriCombinationsOf x y = [ [a, b, c] | a <- [x..y], b <- [x..y], c <- [x..y]]

-- | Checks if String x is in String y, returns True of False
match :: String -> String -> Bool
match x y
        | isJust (find (==x) (words y)) = True
        | otherwise = False

-- | Functions to convert text to a list of corresponding ascii values and vice versa
textToCodes :: String -> [Int]
textToCodes x = [ord y | y <- x]
codesToText :: [Int] -> String
codesToText x = [chr y | y <- x]

listDivisors :: Integer -> [Integer]
listDivisors x = [y | y <- [1..x], x `mod` y == 0]

mergeLists :: (Eq a) => [[a]] -> [a]
mergeLists [] = []
mergeLists (x:y:zs) = [] ++ (union x y) ++ if length zs == 1 then head zs else if length zs == 0 then [] else mergeLists zs