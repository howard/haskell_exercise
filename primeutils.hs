module Primeutils where

primeFactors x
            | x < 2 = error "You must not use a number lower than two as an argument."
            | otherwise = [y | y <- [2..x], x `mod` y == 0]

isPrime x
        | x < 2 = error "You must not use a number lower than two as an argument."
        | (firstPrimeFactor x) == x = True
        | otherwise = False

primesBetween x y = [n | n <- [x..y], isPrime n == True]

firstPrimeFactor x = (primeFactors x) !! 0

primes = [n | n <- [2,3..], isPrime n == True]

filterPrimesBy (x) = [n | n <- [2,3..], isPrime n == True, x n == True]