module Shared where

fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--simple approach
triangleNo x = sum [1..x]
--recursive approach
triangleNo' 1 = 1
triangleNo' x = x + triangleNo' (x-1)

factorial x = if x > 0 then product [1..x] else 0

pentagonalNo x = x * (3 * x - 1) `div` 2

hexagonalNo x = x * (2 * x - 1)