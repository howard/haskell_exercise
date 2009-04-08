{-n! means n × (n − 1) × ... × 3 × 2 × 1

Find the sum of the digits in the number 100!-}


import Data.Char(digitToInt)

euler20 = sum [digitToInt x | x <- (show (product [1..100]))] --correct