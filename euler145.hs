{-Some positive integers n have the property that the sum [ n + reverse(n) ] consists
entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313.
We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading
zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (10^(9))?-}

import Data.Char(digitToInt)

check :: String -> Bool
check [] = True
check (x:y) = if odd (digitToInt x) then check y else False

euler145 = length [x | x <- [10..99999999], check (show (sum [x, read (reverse (show x))::Int]))]