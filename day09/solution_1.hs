module Main where

type Series = [Int]

readSensorData :: String -> [Series]
readSensorData = map (map read <$> words) . lines

predictNextValue :: Series -> Int
predictNextValue = predict . reverse
  where predict []                        = error "unpredictable!"
        predict s@(x:xs)  | all (== 0) s  = 0
                          | otherwise     = x + predict (zipWith (-) s xs)

main :: IO ()
main = interact $ show . sum . map predictNextValue . readSensorData
