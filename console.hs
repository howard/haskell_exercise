{-Useless prompt to proof that Haskell doesn't need loops.-}


prompt = do
  putStr ">> "
  input <- getLine
  if check input == "EXIT" then putStrLn "Exiting..."
    else prompt

prompt' = do
  putStr ">> "
  interact check

check x
      | x == ":q" = "EXIT\n>> "
      | x == ":h" = "Nothing to help here.\n>> "
      | otherwise = x ++ "\n>> "

main = prompt'