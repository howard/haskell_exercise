{-The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.-}


import Primeutils

euler10 = sum [x | x <- primes, x < 2000000]

main = print $ euler10