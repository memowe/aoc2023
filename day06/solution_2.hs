module Main where

import Data.Char
import Text.ParserCombinators.ReadP

type    Time      = Int
type    TimeHiRes = Double
type    Distance  = Int
data    Race      = Race Time Distance deriving Show
type    Range a   = (a, a)

instance Read Race where
  readsPrec _ = readP_to_S races
    where races   = Race <$> timeP <*> distP
          timeP   = string "Time:"      >> digSps
          distP   = string "Distance:"  >> digSps
          digSps  = read . filter isDigit . concat <$>
                    many1 (skipSpaces >> munch1 isDigit) <* char '\n'

beatingTimeRange :: Race -> Range TimeHiRes
beatingTimeRange (Race time distance) =
  let t = fromIntegral time
      d = fromIntegral distance
      a = sqrt $ (t / 2) ** 2 - d
  in  (t/2 - a, t/2 + a)

intLength :: Range TimeHiRes -> Int
intLength (lo, hi) = ceiling (hi-1) - floor (lo+1) + 1

main :: IO ()
main = do
  race <- read <$> getContents
  print $ intLength . beatingTimeRange $ race
