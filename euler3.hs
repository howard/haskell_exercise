{-The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?-}


import Primeutils(isPrime)

euler3 = head [n | n <- [600851475143,600851475142..], isPrime n, 600851475143 `mod` n == 0]

main = print euler3 