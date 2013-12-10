-- Return a list of integers up to a certain value which are prime
-- Using the Sieve of Eratosthenes

import System.Environment (getArgs)
import Data.List

unfactorable :: Integer -> Integer -> Bool
unfactorable y x = if x `mod` y == 0 then
                   False
                 else
                   True

generateSieve :: Integer -> [Integer] -> [Integer]
generateSieve cur (x:xs) = let curIter = filter (unfactorable cur) ( xs )
                                         in x : curIter `intersect` ( generateSieve (head xs) curIter)
generateSieve cur [] = []

sieve :: Integer -> [Integer]
sieve max = 2 : generateSieve 3 [3,5..max]

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