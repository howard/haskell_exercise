module Sudoku where

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

-- | A sample grid plus the matching solution for testing the algorithm.
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

-- | Does the same as checkRow, only with the square whose upper left field is specified by x and y
checkSquare :: [[Int]] -> Int -> Int -> Bool
checkSquare g x y = condition [((g!!x)!!y),((g!!x)!!(y+1)),((g!!x)!!(y+2)),((g!!(x+1))!!y),((g!!(x+1))!!(y+1)),((g!!(x+1))!!(y+2)),((g!!(x+2))!!y),((g!!(x+2))!!(y+1)),((g!!(x+2))!!(y+2))]

-- | Takes a list of integers, usually from checkRow or checkSquare, and returns whether the sudoku
-- | condition of containing only unique numbers from 1 to 9 is fulfilled or not.
condition :: [Int] -> Bool
condition li = length (nub li) == 9 && not (0 `elem` li)

-- | Sudoku puzzle solving algorithm
solve :: [[Int]] -> [[Int]]
solve x = x

-- | Test for the solving algorithm, which compares a sample sudoku puzzle with a solution
-- | that is proven to be correct. Returns whether the computed result matches the correct one or not.
test :: [[Int]] -> [[Int]] -> Bool
test x y = x == y
test' = test (solve sample_grid) sample_grid'