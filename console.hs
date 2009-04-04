{-Useless prompt to proof that Haskell doesn't need loops.-}


prompt = do
  putStr ">> "
  input <- getLine
  if input == "exit" then putStrLn "Exiting..."
    else prompt

main = prompt