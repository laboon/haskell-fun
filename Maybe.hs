import Data.Maybe

fraction :: Int -> Int -> Maybe Float
fraction x 0 = Nothing
fraction x y = Just $ (fromIntegral x) / (fromIntegral y)

main = do
  putStrLn $ "1 / 2 = "
  putStrLn $ show $ fraction 1 2
  putStrLn $ "4 / 0 = "
  putStrLn $ show $ fraction 4 0
