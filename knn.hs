-- Implementation of the KNN algorithm in Haskell

import Data.Ord
import Data.List
import Data.Function

data Element = Element {  classification :: String
                        , location :: [Integer]
                       } deriving (Eq, Show, Ord)

euclideanDistance :: [Integer] -> [Integer] -> Double
euclideanDistance p1 p2 = sqrt
                          . fromIntegral
                          . foldr (+) 0 $ map (^2) $ zipWith (-) p1 p2

comparingDist :: [Integer] -> Element -> Double
comparingDist p e = euclideanDistance p $ location e

sortByDistance :: [Integer] -> [Element] -> [Element]
sortByDistance p elems = sortBy (compare `on` comparingDist p) elems

closestNeighbors :: Integer -> [Integer] -> [Element] -> [Element]
closestNeighbors num p elems = take (fromIntegral num) $ sortByDistance p elems

classify :: Integer -> [Integer] -> [Element] -> String
classify num p elems = show $ closestNeighbors num p elems

main = do

  let e = [Element "Elf" [1,2,1],
           Element "Elf" [1,3,3],
           Element "Elf" [1,2,3],
           Element "Elf" [1,1,2],
           Element "Elf" [2,2,2],
           Element "Gnome" [5,5,5],
           Element "Gnome" [7,6,5],
           Element "Gnome" [8,8,8],
           Element "Gnome" [2,3,4],
           Element "Gnome" [3,2,1]]

  putStrLn "Enter point: "
  p <- getLine
  putStrLn "Enter num neighbors: "
  n <- getLine
  putStrLn $ "Classification is: " ++ (show (classify (read n :: Integer) (read p :: [Integer]) e))