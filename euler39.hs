{-If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?-}

perimeter :: Int -> Int -> Int -> Int
perimeter a b c = (a*b*c) `div` ((4*a*b) `div` 2)

maxSolution = maximum [length [(a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], a^2 + b^2 == c^2, perimeter a b c == x] | x <- [1..1000]]

euler39 = False

main = do
  let triple = maxSolution
  print $ perimeter (triple!!0) (triple!!1) (triple!!2)