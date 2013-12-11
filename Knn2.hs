-- Improved implementation of the kNN algorithm in Haskell
-- The k-Nearest Neighbors algorithm classifies an element according to
-- the mode of the classification of the k nearest neighbors.
import Data.Ord
import Data.List
import Data.Function
import qualified Data.Vector as V

data Element = Element {  classification :: String
                        , location :: V.Vector Integer
                       } deriving (Eq, Show, Ord)

-- Get the most common element of a list.
-- Note this is not the most efficient algorithm, but since size of lists will usually be <10,
-- this is not a huge issue.
getMostCommonElement :: [Element] -> String
getMostCommonElement elems =  head . head . reverse $ sortBy (comparing length) $ group . sort $ map (classification) elems

-- Note that for performance reasons, this does not do the final square root of the distance
-- value.  This is because for comparisons, the relative distance is all that matters,
-- not absolute, and sqrt() does not change the relevant ordering.
euclideanDistance :: V.Vector Integer -> V.Vector Integer -> Double
euclideanDistance p1 p2 = fromIntegral . V.foldr (+) 0 $ V.map (^2) $ V.zipWith (-) p1 p2

comparingDist :: V.Vector Integer -> Element -> Double
comparingDist p e = euclideanDistance p $ location e

sortByDistance :: V.Vector Integer -> [Element] -> [Element]
sortByDistance p elems = sortBy (compare `on` comparingDist p) elems

closestNeighbors :: Integer -> V.Vector Integer -> [Element] -> [Element]
closestNeighbors num p elems = take (fromIntegral num) $ sortByDistance p elems

classify :: Integer -> V.Vector Integer -> [Element] -> String
classify num p elems = show $ getMostCommonElement $ closestNeighbors num p elems

main = do

  let e = [Element "Elf" (V.fromList [1,2,1]),
           Element "Elf" (V.fromList [1,3,3]),
           Element "Elf" (V.fromList [1,2,3]),
           Element "Elf" (V.fromList [1,1,2]),
           Element "Elf" (V.fromList [4,5,5]),
           Element "Gnome" (V.fromList [5,5,5]),
           Element "Gnome" (V.fromList [7,6,5]),
           Element "Gnome" (V.fromList [8,8,8]),
           Element "Gnome" (V.fromList [2,3,4]),
           Element "Gnome" (V.fromList [3,2,1])]


  putStrLn "Enter 3D point in format [x,y,z]: "
  p <- getLine
  putStrLn "Enter num neighbors: "
  n <- getLine
  putStrLn $ "Classification is: "
    ++ (classify (read n :: Integer) (V.fromList (read p :: [Integer])) e)