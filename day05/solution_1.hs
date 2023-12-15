module Main where

import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Control.Applicative
import Control.Monad

type MapRange = (Int, Int, Int) -- from, to, length
type Mapping  = [MapRange]
data Almanac  = Almanac
  { seeds     :: [Int]
  , mappings  :: [Mapping]
  } deriving Show

instance Read Almanac where
  readsPrec _ = readP_to_S almanacP
    where almanacP  = Almanac <$> seedsP <*> mappingP `sepBy1` char '\n'
          seedsP    = between (string "seeds: ") (string "\n\n") numsP
          numsP     = map read <$> munch1 isDigit `sepBy1` char ' '
          mappingP  = do  void $ munch1 (/= ' ') >> string " map:\n"
                          rangesP `endBy1` char '\n'
          rangesP   = do  [to, from, len] <- numsP
                          return (from, to, len)

rangesMap :: Mapping -> Int -> Int
rangesMap mrs x = fromMaybe x $ asum $ rangeMap <$> mrs
  where rangeMap (f,t,l)  | x >= f && x < f + l = Just $ x + t - f
                          | otherwise           = Nothing

chainMap :: [Mapping] -> Int -> Int
chainMap ms x = foldl (flip rangesMap) x ms

finalSeeds :: Almanac -> [Int]
finalSeeds alm = map (chainMap $ mappings alm) (seeds alm)

main :: IO ()
main = do
  finals <- finalSeeds . read <$> getContents
  print $ minimum finals
