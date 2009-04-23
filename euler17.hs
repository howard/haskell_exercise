{-If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of
"and" when writing out numbers is in compliance with British usage.-}


numberConverter x = do
  f (show x) where f c
                     | c == "11" = "eleven"
                     | c == "12" = "twelve"
                     | c == "13" = "thirteen"
                     | c == "14" = "fourteen"
                     | c == "15" = "fifteen"
                     | c == "16" = "sixteen"
                     | c == "17" = "seventeen"
                     | c == "18" = "eighteen"
                     | c == "19" = "nineteen"
                     | (length c) == 1 = (verbalizeNo (c !! 0) False False)
                     | (length c) == 2 = (verbalizeNo (c !! 0) True False) ++ (verbalizeNo (c !! 1) False False)
                     | (length c) == 3 = (verbalizeNo (c !! 0) False True) ++ (verbalizeNo (c !! 1) True False) ++ (verbalizeNo (c !! 2) False False)
                     | c == "1000" = "onethousand"
                     | otherwise = error "Given number too big."
  
verbalizeNo x y z
            | x == '0' = ""
            | x == '1' = if y == True then "ten" else if z == True then "onehundredand" else "one"
            | x == '2' = if y == True then "twenty" else if z == True then "twohundredand" else "two"
            | x == '3' = if y == True then "thirty" else if z == True then "threehundredand" else "three"
            | x == '4' = if y == True then "forty" else if z == True then "fourhundredand" else "four"
            | x == '5' = if y == True then "fifty" else if z == True then "fivehundredand" else "five"
            | x == '6' = if y == True then "sixty" else if z == True then "sixhundredand" else "six"
            | x == '7' = if y == True then "seventy" else if z == True then "sevenhundredand" else "seven"
            | x == '8' = if y == True then "eighty" else if z == True then "eighthundredand" else "eight"
            | x == '9' = if y == True then "ninety" else if z == True then "ninehundredand" else "nine"
            | otherwise = error "Dont't give me letterz, dawg."

lengthOfPronouncedNumbers n m = sum [length (numberConverter x) | x <- [n..m]]
euler17 = lengthOfPronouncedNumbers 1 1000
