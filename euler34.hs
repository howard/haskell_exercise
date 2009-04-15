{-145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.-}


import Data.Char(digitToInt)
import Shared(factorial)

euler34 = sum [x | x <- [3..100000000], x == sum [factorial (digitToInt y) | y <- show(x)]]