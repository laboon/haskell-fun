import System.Environment (getArgs)
import Data.List

-- Return a list of integers up to a certain value which are prime
-- Using the Sieve of Eratosthenes
sieve :: Integer -> [Integer]
sieve max = [1]

convertInt :: String -> Integer
convertInt x = (read x :: Integer)

main = do
  args <- getArgs
  pure_args <- return args
  if (null args) then do
    error "You need to provide a maximum number!"
  else if (length args > 1) then do
    error "You need to provide only one argument!"
  else do
    let max = convertInt (head args)
    putStrLn $ show $ sieve max