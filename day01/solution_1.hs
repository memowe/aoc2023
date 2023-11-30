module Main where

process :: String -> String
process input = "Hello from day 01 / 1\nInput was " ++ input

main :: IO ()
main = interact process
