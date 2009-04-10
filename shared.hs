module Shared where

fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--simple approach
triangleNo x = sum [1..x]

--recursive approach
triangleNo' 1 = 1
triangleNo' x = x + triangleNo' (x-1)
