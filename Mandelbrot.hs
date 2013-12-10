import Graphics.Gnuplot.Simple
import Data.Complex

mandelbrot :: Complex Double -> [Complex Double]
mandelbrot n = iterate (\x -> x^2 + n) n

shouldShow :: Complex Double -> Bool
shouldShow x
  | (magnitude ((mandelbrot x) !! 50)) < 2 = True
  | otherwise = False

allPoints :: [(Double, Double)]
allPoints = [ (x,y) | x <- [-2, -1.999 .. 0.5], y <- [1, 0.999 .. -1]]

mandelbrotPoints :: [(Double, Double)]
mandelbrotPoints = filter (\p -> (shouldShow ((fst p) :+ (snd p)))) allPoints

main = do
  plotListStyle [] (defaultStyle{plotType = Dots}) mandelbrotPoints