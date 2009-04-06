{-By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6^(th) prime is 13.

What is the 10001^(st) prime number?-}


prims n = 2:[x | x <- [3..], odd x] !! n
prime n = prims !! n where even ((prims !! n) - (prims !! n-1))

euler7 = prime 1000
