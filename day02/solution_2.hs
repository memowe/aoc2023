module Main where

process :: String -> String
process = id

main :: IO ()
main = interact process
