{-If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.-}


variant1 = sum [x | x <- [3..999], (x `mod` 3) == 0] + sum [x | x <- [5..999], (x `mod` 5) == 0] --fail

variant2 = sum [3,6..999] + sum [5,10..995] --fail

-- variant1 and variant2 probably fail because multiples of 3 AND 5 are added twice.

variant3 = sum  [x | x <- [3..999], (x `mod` 3) == 0 || (x `mod` 5) == 0] --win

euler1 = variant3 --correct
