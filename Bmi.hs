cmToM :: (Fractional a) => a -> a
cmToM cm = cm / 100

bmi :: (RealFrac a) => a -> a -> a
bmi weight height = weight / (cmToM height) ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | (bmi weight height) <= 18.5 = "You're underweight!"
  | (bmi weight height) <= 25.0 = "You're average!"
  | (bmi weight height) <= 30.0 = "You're overweight!"
  | otherwise                   = "You're very overweight!"

main = do
  putStrLn "Enter weight (in kilograms)"
  val <- getLine
  weight <- return val
  putStrLn "Enter height (in centimeters)"
  val <- getLine
  height <- return val
  putStrLn "Your BMI is: "
  putStrLn $ show $ bmi (read weight) (read height)
