module Sudoku where

import Data.List(nub)
import Shared(take')

-- | Sudoku solver

-- | Makes sure that the type of the values in the grid is always Int, not accidently Integer
grid :: [Int] -> [Int]
grid x = x

sampleGrid = grid  [0,3,0,0,0,0,0,0,0,
                    0,0,0,1,9,5,0,0,0,
                    0,9,8,0,0,0,0,6,0,
                    8,0,0,0,6,0,0,0,0,
                    4,0,0,0,0,3,0,0,1,
                    0,0,0,0,2,0,0,0,0,
                    0,6,0,0,0,0,2,8,0,
                    0,0,0,4,1,9,0,0,5,
                    0,0,0,0,0,0,0,7,0]
sampleGrid' = grid [5,3,4,6,7,8,9,1,2,
                    6,7,2,1,9,5,3,4,8,
                    1,9,8,3,4,2,5,6,7,
                    8,5,9,7,6,1,4,2,3,
                    4,2,6,8,5,3,7,9,1,
                    7,1,3,9,2,4,8,5,6,
                    9,6,1,5,3,7,2,8,4,
                    2,8,7,4,1,9,6,3,5,
                    3,4,5,2,8,6,1,7,9]

-- | Takes a list of integers, usually from checkRow or checkSquare, and returns whether the sudoku
-- | condition of containing only unique numbers from 1 to 9 is fulfilled or not.
condition :: [Int] -> Bool
condition li = length (nub li) == 9 && not (0 `elem` li)

-- | Determines the side-length of the sudoku grid.
sideLen :: [Int] -> Int
sideLen g
        | len `mod` 18 == 0 = 18
        | len `mod` 15 == 0 = 15
        | len `mod` 12 == 0 = 12
        | len `mod` 9 == 0 = 9
        | len `mod` 6 == 0 = 6
        | len `mod` 3 == 0 = 3
        | otherwise = error "This program can't deal with sudoku puzzles with the given length."
        where len = length g

-- | Checks whether all values in row n of grid g are unique or not.
checkRow :: [Int] -> Int -> Bool
checkRow g n = condition (take' g (0+n*len) (8+n*len))
                where len = sideLen g

-- | Same as checkRow, only vertically.
checkColumn :: [Int] -> Int -> Bool
checkColumn g n = condition [g !! ((sideLen g) * n)| n <- [0..((sideLen g) - 1)]]

-- | Same as checkRow, only with squares.
checkSquare :: [Int] -> Int -> Bool
checkSquare g n = condition [g !! (n*3), g !! (n*3 + 1), g !! (n*3 + 2), 
                             g !! (n*3 + len), g !! (n*3 + 1 + len), g !! (n*3 + 2 + len),
                             g !! (n*3 + len*2), g !! (n*3 + 1 + len*2), g !! (n*3 + 2 + len*2)] where len = sideLen g