module Main where

import Data.Char
import Data.Map (Map, fromList, keys)
import Text.ParserCombinators.ReadP
import Lens.Micro.Platform
import Control.Monad.State.Lazy

-- NOTE:  This code tries to solve using the LCM of all input loop lengths
--        but that's not a general solution, it just works for the input.
--        https://www.reddit.com/r/adventofcode/comments/18dfpub/2023_day_8_part_2_why_is_spoiler_correct

data    Dir     = L | R deriving (Eq, Show, Enum, Ord, Bounded)
type    Node    = (Char, Char, Char)
type    Network = Map Node (Map Dir Node)
data    MapDoc  = MapDoc  { _instructions :: [Dir]
                          , _network      :: Network
                          } deriving Show

makeLenses ''MapDoc

isStart, isFinish :: Node -> Bool
isStart   (_,_,c) = c == 'A'
isFinish  (_,_,c) = c == 'Z'

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
          nodeP     = do  [a,b,c] <- count 3 $ satisfy isAlphaNum
                          return (a,b,c)

data MapState = MS
  { _nextInstructions :: [Dir]
  , _networkMap       :: Network
  , _currentNode      :: Node
  }

makeLenses ''MapState

loopLength :: MapState -> Int
loopLength = evalState walkLoop
  where walkLoop = do
          node  <- use currentNode
          dir   <- head <$> use nextInstructions
          next  <- (^?! ix node . ix dir) <$> use networkMap
          if isFinish next
            then return 1
            else do
              nextInstructions %= tail
              currentNode .= next
              (+1) <$> walkLoop

main :: IO ()
main = do
  mapDoc <- read <$> getContents
  print $ lcmm . map (ll mapDoc) . filter isStart <$> keys $ mapDoc^.network
  where ll md = loopLength . MS (cycle $ md^.instructions) (md^.network)
        lcmm  = foldr lcm 1
