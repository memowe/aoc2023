module Main where

import Data.Char
import Data.Map (Map, fromList)
import Text.ParserCombinators.ReadP
import Lens.Micro.Platform
import Control.Monad.State.Lazy

data    Dir     = L | R deriving (Eq, Show, Enum, Ord, Bounded)
type    Node    = (Char, Char, Char)
type    Network = Map Node (Map Dir Node)
data    MapDoc  = MapDoc  { _instructions :: [Dir]
                          , _network      :: Network
                          } deriving Show

makeLenses ''MapDoc

start, finish :: Node
start = ('A','A','A'); finish = ('Z','Z','Z')

instance Read MapDoc where
  readsPrec _ = readP_to_S mapdocP
    where mapdocP   = MapDoc <$> instrsP <*> (string "\n\n" >> networkP)
          instrsP   = many1.choice $ instrP <$> [minBound..]
          instrP d  = d <$ string (show d)
          networkP  = fromList <$> many nwLineP
          nwLineP   = do  from  <- nodeP <* string " = "
                          tos   <- between (char '(') (string ")\n") nodeMap
                          return (from, tos)
          nodeMap   = fromList . zip [minBound..] <$> nodeP `sepBy` string ", "
          nodeP     = do  [a,b,c] <- count 3 $ satisfy isLetter
                          return (a,b,c)

data MapState = MS
  { _nextInstructions :: [Dir]
  , _currentNode      :: Node
  , _networkMap       :: Network
  }

makeLenses ''MapState

walk :: State MapState Int
walk = do
  node  <- use currentNode
  dir   <- head <$> use nextInstructions
  next  <- (^?! ix node . ix dir) <$> use networkMap
  if next == finish
    then return 1
    else do
      nextInstructions %= tail
      currentNode .= next
      (+1) <$> walk

main :: IO ()
main = do
  mapDoc <- read <$> getContents
  print $ evalState walk $
    MS (cycle $ mapDoc^.instructions) start (mapDoc^.network)
