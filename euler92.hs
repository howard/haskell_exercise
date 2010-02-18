{-A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

For example,

44  32  13  10  1  1
85  89  145  42  20  4  16  37  58  89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?-}

import Data.Char(digitToInt)

digits x = [digitToInt a | a <- (show x), a /= '\"']

coolChain 1 = 1
coolChain 89 = 89
coolChain n = coolChain(sum [dig ^ 2 | dig <- digits n])

euler92 = length [a | a <- [2..10000000], coolChain a == 89]

main = print euler92 --correct 1m26.641s