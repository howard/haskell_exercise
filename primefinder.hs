module Main(main) where

import Data.Char(digitToInt)
import System(getArgs)
import Primeutils(primes)

main = do
  args <- getArgs
  if (length args) == 0 then
    error "You must specify a number n as argument to get the nth prime number."
    else
      if (args !! 0) < 1 then
        error "You must specify a number larger than 0."
        else
          primes !! ((digitToInt ((args !! 0) !! 0)) - 1)