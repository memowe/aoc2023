module Main where

type Series = [Int]

readSensorData :: String -> [Series]
readSensorData = map (map read <$> words) . lines

predict :: Series -> Int
predict []                        = error "unpredictable!"
predict s@(x:xs)  | all (== 0) s  = 0
                  | otherwise     = x - predict (zipWith (-) xs s)

main :: IO ()
main = interact $ show . sum . map predict . readSensorData
