{-The number 3797 has an interesting property. Being prime itself, it is possible to continuously
remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly
we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.-}


import Data.Char(digitToInt)
import Primeutils(isPrime)

checkTruncsLeft :: Int -> Bool
checkTruncsLeft x
                | length y < 2 = isPrime x
                | otherwise = if isPrime (digitToInt (head y)) then
                  checkTruncsLeft (read (tail y)::Int) else False
                where y = show x

checkTruncsRight :: Int -> Bool
checkTruncsRight x
                 | length y < 2 = if x == 0 then False else isPrime x
                 | otherwise = if (last y) /= '0' then if isPrime (digitToInt (last y)) then checkTruncsRight (read (reverse (tail y))::Int) else False else False
                 where y = show x

euler37 = sum $ take 11 [x | x <- [11,12..], isPrime x, checkTruncsLeft x, checkTruncsRight x]

main = print euler37