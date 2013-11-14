quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger  = quicksort [a | a <- xs, a > x]
  in smaller ++ [x] ++ bigger

convertInts :: [String] -> [Integer]
convertInts [] = []
convertInts (x:xs) = (read x :: Integer) : convertInts xs

main = do
  putStrLn "Enter a list of #s separated by spaces to sort, just CR to quit"
  line <- getLine
  if null line
     then do
        putStrLn "done!"
     else do
        rl <- return line
        let nums = convertInts $ words rl
        putStrLn $ show $ quicksort $ nums
        main
