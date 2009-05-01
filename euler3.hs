{-The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?-}


import Primeutils(isPrime)
import Shared(listDivisors)

euler3 = head [putStrLn (show x) | x <- (reverse (listDivisors 600851475143)), isPrime x]

main = euler3