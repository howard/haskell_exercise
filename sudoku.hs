--module Sudoku where

import Data.Char(intToDigit)
import Data.List(nub)

-- | 9x9 Sudoku solver

grid :: [[Int]]
grid = [[0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0] ]

-- | A sample grid plus the matching solution for testing the solving function
sample_grid :: [[Int]]
sample_grid = [[0,3,0,0,0,0,0,0,0],
               [0,0,0,1,9,5,0,0,0],
               [0,9,8,0,0,0,0,6,0],
               [8,0,0,0,6,0,0,0,0],
               [4,0,0,0,0,3,0,0,1],
               [0,0,0,0,2,0,0,0,0],
               [0,6,0,0,0,0,2,8,0],
               [0,0,0,4,1,9,0,0,5],
               [0,0,0,0,0,0,0,7,0] ]

sample_grid' :: [[Int]]
sample_grid' = [[5,3,4,6,7,8,9,1,2],
                [6,7,2,1,9,5,3,4,8],
                [1,9,8,3,4,2,5,6,7],
                [8,5,9,7,6,1,4,2,3],
                [4,2,6,8,5,3,7,9,1],
                [7,1,3,9,2,4,8,5,6],
                [9,6,1,5,3,7,2,8,4],
                [2,8,7,4,1,9,6,3,5],
                [3,4,5,2,8,6,1,7,9] ]

-- | Checks if a sudoku puzzle is solved by looking for zeros.
checkIfSolved :: [[Int]] -> Bool
checkIfSolved g = all (==True) $
                    [checkRow g row | row <- [0..8]] ++ [checkSquare g x y | x <- [0,3,6], y <- [0,3,6]]

-- | Checks whether all values in row n of grid g are unique or not
checkRow :: [[Int]] -> Int -> Bool
checkRow g n = condition (g !! n)

-- | Same as checkRow, only vertically
checkColumn :: [[Int]] -> Int -> Bool
checkColumn g n = condition [(g !! a) !! n | a <- [0..8]]

-- | Does the same as checkRow, only with the square whose upper left field is specified by x and y
checkSquare :: [[Int]] -> Int -> Int -> Bool
checkSquare g x y = condition [((g!!x)!!y),((g!!x)!!(y+1)),((g!!x)!!(y+2)),((g!!(x+1))!!y),((g!!(x+1))!!(y+1)),((g!!(x+1))!!(y+2)),((g!!(x+2))!!y),((g!!(x+2))!!(y+1)),((g!!(x+2))!!(y+2))]

-- | Takes a list of integers, usually from checkRow or checkSquare, and returns whether the sudoku
-- | condition of containing only unique numbers from 1 to 9 is fulfilled or not.
condition :: [Int] -> Bool
condition li = length (nub li) == 9 && not (0 `elem` li)

-- | Fills a square with the upper-left-corner coordinates x y in grid g with the 9 numbers in v.
tryValuesOnSquare :: [Int] -> [[Int]] -> Int -> Int -> [[Int]]
tryValuesOnSquare v g x y
                | x==0&&y==0=[[f ((g!!0)!!0) (v!!0),f ((g!!0)!!1) (v!!1),f ((g!!0)!!2) (v!!2),(g!!0)!!3,(g!!0)!!4,(g!!0)!!5,(g!!0)!!6,(g!!0)!!7,(g!!0)!!8],
                              [f ((g!!1)!!0) (v!!3),f ((g!!1)!!1) (v!!4),f ((g!!1)!!2) (v!!5),(g!!1)!!3,(g!!1)!!4,(g!!1)!!5,(g!!1)!!6,(g!!1)!!7,(g!!1)!!8],
                              [f ((g!!2)!!0) (v!!6),f ((g!!2)!!1) (v!!7),f ((g!!2)!!2) (v!!8),(g!!2)!!3,(g!!2)!!4,(g!!2)!!5,(g!!2)!!6,(g!!2)!!7,(g!!2)!!8],
                              g!!3,
                              g!!4,
                              g!!5,
                              g!!6,
                              g!!7,
                              g!!8 ]
                | x==1&&y==1=[g!!0,
                              g!!1,
                              g!!2,
                              [(g!!3)!!0,(g!!3)!!1,(g!!3)!!2,f ((g!!3)!!3) (v!!0),f ((g!!3)!!4) (v!!1),f ((g!!3)!!5) (v!!2),(g!!3)!!6,(g!!3)!!7,(g!!3)!!8],
                              [(g!!4)!!0,(g!!4)!!1,(g!!4)!!2,f ((g!!4)!!3) (v!!3),f ((g!!4)!!4) (v!!4),f ((g!!4)!!5) (v!!5),(g!!4)!!6,(g!!4)!!7,(g!!4)!!8],
                              [(g!!5)!!0,(g!!5)!!1,(g!!5)!!2,f ((g!!5)!!3) (v!!6),f ((g!!5)!!4) (v!!7),f ((g!!5)!!5) (v!!8),(g!!5)!!6,(g!!5)!!7,(g!!5)!!8],
                              g!!6,
                              g!!7,
                              g!!8 ]
                | x==2&&y==2=[g!!0,
                              g!!1,
                              g!!2,
                              g!!3,
                              g!!4,
                              g!!5,
                              [(g!!6)!!0,(g!!6)!!1,(g!!6)!!2,(g!!6)!!3,(g!!6)!!4,(g!!6)!!5,f ((g!!6)!!6) (v!!0),f ((g!!6)!!7) (v!!1),f ((g!!6)!!8) (v!!2)],
                              [(g!!7)!!0,(g!!7)!!1,(g!!7)!!2,(g!!7)!!3,(g!!7)!!4,(g!!7)!!5,f ((g!!7)!!6) (v!!3),f ((g!!7)!!7) (v!!4),f ((g!!7)!!8) (v!!5)],
                              [(g!!8)!!0,(g!!8)!!1,(g!!8)!!2,(g!!8)!!3,(g!!8)!!4,(g!!8)!!5,f ((g!!8)!!6) (v!!6),f ((g!!8)!!7) (v!!7),f ((g!!8)!!8) (v!!8)] ]
                | otherwise = error "This function doesn't conver the square you requested."
                where f a b = if a /= 0 then a else b -- checks if there is already a value given

-- | Generates a list of all possible combinations of nine distinct numbers.
generateNine :: [[Int]]
generateNine = [nineNos | a <- [1..9], b <- [1..9], c <- [1..9], d <- [1..9], e <- [1..9], f <- [1..9], g <- [1..9], h <- [1..9], i <- [1..9], let nineNos = [a,b,c,d,e,f,g,h,i], length (nub nineNos) == 9]

-- | Sudoku puzzle solving function.
solve :: [[Int]] -> [[Int]]
solve x = do
  -- brute-force upper left square, respecting sudoku rules horizontally and vertically as well
  let sq00 = head [g | nineNos <- generateNine, let g = tryValuesOnSquare nineNos x 0 0,
                          checkSquare g 0 0, all (==True) [checkRow g n | n <- [0,1,2]], all (==True) [checkColumn g n | n <- [0,1,2]]]
  -- brute-force lower left square, respecting sudoku rules horizontally and vertically as well
  let sq22 = head [g | nineNos <- generateNine, let g = tryValuesOnSquare nineNos sq00 2 2,
                          checkSquare g 2 2, all (==True) [checkRow g n | n <- [6,7,8]], all (==True) [checkColumn g n | n <- [6,7,8]]]
  -- brute-force center square, respecting sudoku rules horizontally and vertically as well
  let sq11 = head [g | nineNos <- generateNine, let g = tryValuesOnSquare nineNos sq22 1 1,
                          checkSquare g 1 1, all (==True) [checkRow g n | n <- [3,4,5]], all (==True) [checkColumn g n | n <- [3,4,5]]]
  -- return result
  sq11

-- | Converts the integer grid to something better readable for humans.
makeGridReadable :: [[Int]] -> [String]
makeGridReadable g = [(show row) | row <- g]

-- | Test for the solving function, which compares a sample sudoku puzzle with a solution
-- | that is proven to be correct. Returns whether the computed result matches the correct one or not.
test :: [[Int]] -> [[Int]] -> Bool
test x y = x == y
test' = test (solve sample_grid) sample_grid'

main = do
  let g = makeGridReadable $ solve sample_grid
  print (g !! 0)
  print (g !! 1)
  print (g !! 2)
  print (g !! 3)
  print (g !! 4)
  print (g !! 5)
  print (g !! 6)
  print (g !! 7)
  print (g !! 8)