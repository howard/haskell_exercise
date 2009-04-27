import Primeutils(primesBetween,eratosthenes)
import System(getArgs)

main = do
  args <- getArgs
  putStrLn "Primeutils benchmark - calculates primes from 2 to 50000. Use the 'time' command to measure differences."
  if length args > 0 then
    case head args of
      "traditional"   -> print (primesBetween 2 50000)
      "eratosthenes"  -> print (eratosthenes 50000)
    else
      putStrLn "Supply either 'traditional' or 'eratosthenes' as argument to determine the calculation method."