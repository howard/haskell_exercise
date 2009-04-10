factorial x = if x > 0 then product [1..x] else 0

factorial' :: Int -> Int
factorial' 0 = 1
factorial' 1 = 1
factorial' x = x * factorial' (x-1)

factorial'' :: Int -> Int
factorial'' x
      | x < 0 = error "You can't make the factorial or a negative number."
      | x == 0 || x == 1 = 1
      | x > 1 = x * factorial'' (x-1)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort all@(_:xs) = (maximum all) : (quicksort xs) --bullshit