{-2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?-}


euler5 = head [x | x <- [2520,2540..], x `mod` 2 == 0, x `mod` 3 == 0, x `mod` 4 == 0, x `mod` 5 == 0, x `mod` 6 == 0,
                                   x `mod` 7 == 0, x `mod` 8 == 0, x `mod` 9 == 0, x `mod` 10 == 0, x `mod` 11 == 0,
                                   x `mod` 12 == 0, x `mod` 13 == 0, x `mod` 14 == 0, x `mod` 15 == 0, x `mod` 16 == 0,
                                   x `mod` 17 == 0, x `mod` 18 == 0, x `mod` 19 == 0] --correct 232792560
