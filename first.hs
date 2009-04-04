doubleMe x = x + x

doubleSmallNumber x = if x > 100
  then x
  else x*2

factorial x = if x > 0 then product [1..x] else 0

swapStringCase str = [swapCharCase c | c <- str]

swapCharCase c = if c `elem` [a..z] then c else c