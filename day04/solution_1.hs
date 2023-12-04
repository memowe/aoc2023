module Main where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data ScratchCard  = SC {winning :: [Int], have :: [Int]}

instance Read ScratchCard where
  readsPrec _ = readP_to_S card
    where card = do
            scWinning <- string "Card" >> sp >> int >> char ':' >> sp >> ints
            scHave    <- sp >> char '|' >> sp >> ints
            return $ SC scWinning scHave
          ints  = int `sepBy` sp
          int   = read <$> munch1 isDigit
          sp    = munch1 (== ' ')

points :: ScratchCard -> Int
points sc = let ws = winning sc `intersect` have sc
            in  ps !! length ws
  where ps = 0 : iterate (*2) 1

main :: IO ()
main = interact $ show . sum . map (points . read) . lines
