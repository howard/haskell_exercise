{-Starting in the top left corner of a 22 grid, there are 6 routes (without backtracking) to the bottom right corner. How many routes are there through a 2020 grid?-}

factorial x = if x > 0 then product [1..x] else 0

-- | compute number of routes through a x*y rectangle
noOfRoutes :: Integer -> Integer -> Integer
noOfRoutes x y = (factorial (x + y)) `div` ((factorial x) * (factorial y))

euler15 :: Integer
euler15 = noOfRoutes 20 20

main = print $ euler15 --correct