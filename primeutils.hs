module Primeutils where

import Shared(mergeLists,listDivisors)

primeFactors x
            | x < 2 = error "You must not use a number lower than two as an argument."
            | otherwise = [y | y <- [2..x], x `mod` y == 0]

isPrime x
        | x < 0 = error "You must not use a number lower than zero as an argument."
        | x == 1 = False
        | (firstPrimeFactor x) == x = True
        | otherwise = False

primesBetween x y = [n | n <- [x..y], isPrime n == True]

firstPrimeFactor x = (primeFactors x) !! 0

primes = [n | n <- [2,3..], isPrime n == True]

filterPrimesBy (x) = [n | n <- [2,3..], isPrime n == True, x n == True]

-- | lists all primes from 2 to the first parameter, using Eratosthenes' method
eratosthenes :: Integer -> [Integer]
eratosthenes x = do
  let pool = 2:[3,5..x]
  let multipleFilter [] = []
  let multipleFilter (x:xs)
                          | length xs == 1 = x:(head xs):[]
                          | otherwise = x : multipleFilter (filter (\y -> (y `mod` x /= 0)) xs)
  multipleFilter pool