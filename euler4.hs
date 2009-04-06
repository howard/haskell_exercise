{-A palindromic number reads the same both ways. The largest palindrome made from the product
of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.-}


toInt x = read (show x)::Int

euler4 = [x*y | x <- [999,998..100], y <- [999,998..100],
  take (toInt (truncate (length (show (x*y))))) (show (x*y)) == take (toInt (truncate (length (show (x*y)))) (reverse (show (x*y))))]
