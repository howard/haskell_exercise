{-The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)-}

isPalindromic :: [Char] -> Bool
isPalindromic x = x == reverse x

toBase2 :: Integer -> [Char]
toBase2 x = reverse (toBase2' x)
toBase2' 0 = ""
toBase2' x = (show (x `rem` 2)) ++ (toBase2' (x `div` 2))

euler36 = sum [x | x <- [1..1000000], (isPalindromic (show x)) == True,
                                      (isPalindromic (toBase2 x)) == True]

main = print euler36 --correct 872187