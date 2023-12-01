module Main where

import Data.Char

extractTwoDigitNumber :: String -> Int
extractTwoDigitNumber cs = read $ head digits : [last digits]
  where digits = filter isDigit cs

process :: String -> String
process = show . sum . map extractTwoDigitNumber . lines

main :: IO ()
main = interact process
