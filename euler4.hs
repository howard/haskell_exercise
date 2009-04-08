{-A palindromic number reads the same both ways. The largest palindrome made from the product
of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.-}



euler4 = maximum [x*y | x <- [999,998..100], y <- [999,998..100], show (x*y) == reverse (show (x*y))] --correct
