{-It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.-}


import Shared(compareUnordList)

euler52 = head [x | x <- [142857..], let x' = show x, compareUnordList x' (show $ 2*x),
  compareUnordList x' (show $ 3*x), compareUnordList x' (show $ 4*x), compareUnordList x' (show $ 5*x),
  compareUnordList x' (show $ 6*x)]

main = print euler52 --correct 142857 in 0.027s