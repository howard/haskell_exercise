{-The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
Although it has not been proved yet (Collatz Problem), it is thought that all starting
numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.-}


import List(findIndex)
import Maybe(fromJust)

sequ x = x : (sequ' x)
sequ' 2 = [1]
sequ' x
      | even x = (x `div` 2) : (sequ' (x `div` 2))
      | odd x = (3*x +1) : (sequ' (3*x +1))

maximum' a b = maximum ([1] ++ [result | x <- [a..b], let result = length (sequ x), result > 500])
euler14 = maximum [(maximum' 1 333333), (maximum' 333334 666666), (maximum' 666667 999999)]

--takes 7 minutes and 16 seconds on my MacBook Pro 15'' to determine the result, but I misinterpreted the task

candidates = [(x, result) | x <- [666667..999999], let result = length (sequ x), result > 500]
candidateValues = [snd x | x <- candidates]
maxCandidate = maximum candidateValues
index = fromJust (findIndex (==maxCandidate) candidateValues)
euler14' = fst (candidates !! index) --correct