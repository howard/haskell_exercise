{-A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation
of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically,
we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?-}


import Data.Char(intToDigit)
import Shared(isUnique)

euler24 = [(intToDigit a):(intToDigit b):(intToDigit c):(intToDigit d):(intToDigit e):(intToDigit f):(intToDigit g):(intToDigit h):(intToDigit i):[(intToDigit j)] |
          a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], e <- [0..9], f <- [0..9], g <- [0..9], h <- [0..9], i <- [0..9], j <- [0..9]] !! 999999 --fail, just keeping it for nostalgic purposes


euler24' = [n | a <- ['0'..'9'], b <- ['0'..'9'], c <- ['0'..'9'], d <- ['0'..'9'],
  e <- ['0'..'9'], f <- ['0'..'9'], g <- ['0'..'9'], h <- ['0'..'9'], i <- ['0'..'9'], j <- ['0'..'9'],
  let n = a:b:c:d:e:f:g:h:i:[j], isUnique a n, isUnique b n, isUnique c n, isUnique d n, isUnique e n,
  isUnique f n, isUnique g n, isUnique h n, isUnique i n, isUnique j n] !! 999999 

main = print euler24' --correct 2783915460 in 17m22.793s