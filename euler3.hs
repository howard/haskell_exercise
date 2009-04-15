{-The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?-}


euler3 = [x | x <- [600851475142,600851475141..2], 600851475143 `mod` x == 0] !! 0