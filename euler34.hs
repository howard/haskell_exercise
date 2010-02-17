{-145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.-}


import Data.Char(digitToInt)

factorial x = if x > 0 then product [1..x] else 0

euler34 = sum [x | x <- [1..1000000], x == sum [factorial (digitToInt y) | y <- show(x)]]

main = print euler34