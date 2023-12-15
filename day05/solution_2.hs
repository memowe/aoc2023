module Main where

import Data.Char
import Data.List
import Data.List.Extra
import Data.Ord
import Data.Maybe
import Text.ParserCombinators.ReadP
import Control.Applicative
import Control.Monad

type SeedRange  = (Int, Int) -- from, length
type MapRange   = (Int, Int, Int) -- from, to, length
type Mapping    = [MapRange]
data Almanac    = Almanac
  { seeds     :: [SeedRange]
  , mappings  :: [Mapping]
  } deriving Show

data Range = R {rFrom :: Int, rTo :: Int} deriving Show

instance Read Almanac where
  readsPrec _ = readP_to_S almanacP
    where almanacP    = Almanac <$> seedsP <*> mappingP `sepBy1` char '\n'
          seedsP      = between (string "seeds: ") (string "\n\n") $
                        seedRangeP `sepBy1` char ' '
          seedRangeP  = (,) <$> numP <*> (char ' ' >> numP)
          mappingP    = do  void $ munch1 (/= ' ') >> string " map:\n"
                            rangesP `endBy1` char '\n'
          rangesP     = do  [to, from, len] <- numP `sepBy1` char ' '
                            return (from, to, len)
          numP        = read <$> munch1 isDigit

seedRangeToRange :: SeedRange -> Range
seedRangeToRange (f,l) = R f (f+l-1)

expandRange :: Range -> [Int]
expandRange = enumFromTo <$> rFrom <*> rTo

joinRanges :: [Range] -> [Range]
joinRanges ranges =
  let sorted  = sortBy (comparing rFrom <> comparing (Down . rTo)) ranges
      dropped = map head . groupOn rFrom $ sorted
  in  reverse $ foldl collapse [] dropped
  where collapse [] r = [r]
        collapse (r@(R f t):rs) r'@(R f' t')
          | f' > t    = r' :  r : rs -- disjoint
          | t' < t    =       r : rs -- r contains r'
          | otherwise = R f t'  : rs -- overlapping

rangesMap :: Mapping -> Int -> Int
rangesMap mrs x = fromMaybe x $ asum $ rangeMap <$> mrs
  where rangeMap (f,t,l)  | x >= f && x < f + l = Just $ x + t - f
                          | otherwise           = Nothing

chainMap :: [Mapping] -> Int -> Int
chainMap ms x = foldl (flip rangesMap) x ms

finalSeeds :: Almanac -> [Int]
finalSeeds alm =
  let ranges    = joinRanges $ map seedRangeToRange $ seeds alm
      allSeeds  = concatMap expandRange ranges
  in  map (chainMap $ mappings alm) allSeeds

main :: IO ()
main = do
  finals <- finalSeeds . read <$> getContents
  print $ minimum finals
