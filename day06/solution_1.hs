module Main where

import Data.Char
import Text.ParserCombinators.ReadP
import Control.Monad

type    Time      = Int
type    TimeHiRes = Double
type    Distance  = Int
data    Race      = Race Time Distance          deriving Show
newtype Races     = Races {getRaces :: [Race]}  deriving Show
type    Range a   = (a, a)

instance Read Races where
  readsPrec _ = readP_to_S races
    where races   = (Races .) . zipWith Race <$> timesP <*> distsP
          timesP  = string "Time:"      >> sp >> (num `sepBy` sp) <* nl
          distsP  = string "Distance:"  >> sp >> (num `sepBy` sp) <* nl
          num     = read <$> munch1 isDigit
          sp      = void $ munch1 isSpace
          nl      = void $ char '\n'

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
  races <- getRaces . read <$> getContents
  print $ product $ map (intLength . beatingTimeRange) races
