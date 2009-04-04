{-If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.-}


variant1 = sum [x | x <- [6..999], (x `mod` 3) == 0] + sum [x | x <- [1..999], (x `mod` 5) == 0]

variant2 = sum [3,6..999] + sum [5,10..995]
