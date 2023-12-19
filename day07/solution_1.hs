module Main where

import Data.Char
import Data.List
import Data.Ord
import Control.Applicative
import Text.ParserCombinators.ReadP

data Card = C2 | C3 | C4 | C5 | C6
          | C7 | C8 | C9 | CT
          | CJ | CQ | CK | CA
          deriving (Eq, Enum, Ord, Bounded)

instance Show Card where
  show = return . ("23456789TJQKA" !!) . fromEnum

newtype Hand = Hand
  { getCards :: (Card, Card, Card, Card, Card)
  } deriving Eq

instance Show Hand where
  show = concatMap show . handToList

handParser :: ReadP Hand
handParser = do
  cards <- (,,,,) <$> cardP <*> cardP <*> cardP <*> cardP <*> cardP
  return $ Hand cards
  where cardP     = choice (cardPOf <$> [minBound..maxBound])
        cardPOf c = string (show c) >> return c

instance Read Hand where
  readsPrec _ = readP_to_S handParser

handToList :: Hand -> [Card]
handToList (Hand (a,b,c,d,e)) = [a,b,c,d,e]

data Type = HighCard
          | OnePair
          | TwoPair
          | ThreeOfAKind
          | FullHouse
          | FourOfAKind
          | FiveOfAKind
          deriving (Eq, Show, Enum, Ord, Bounded)

typeOfHand :: Hand -> Type
typeOfHand h = case handGroups of
  (5:_)     -> FiveOfAKind
  (4:_)     -> FourOfAKind
  (3:2:_)   -> FullHouse
  (3:_)     -> ThreeOfAKind
  (2:2:_)   -> TwoPair
  (2:_)     -> OnePair
  _         -> HighCard
  where handGroups = sortOn Down . map length . group . sort . handToList $ h

instance Ord Hand where
  compare = liftA2 (<>) <$> comparing typeOfHand
                        <*> comparing getCards

data HandBid = HandBid
  { hand  :: Hand
  , bid   :: Int
  } deriving Eq

instance Show HandBid where
  show (HandBid h b) = show h ++ " " ++ show b

handBidParser :: ReadP HandBid
handBidParser = HandBid <$> handParser <*> (char ' ' >> int)
  where int = read <$> munch1 isDigit

instance Read HandBid where
  readsPrec _ = readP_to_S handBidParser

instance Ord HandBid where
  compare = comparing hand

newtype Camel = Camel
  { getHandBids :: [HandBid]
  } deriving Show

camelParser :: ReadP Camel
camelParser = Camel <$> many1 (handBidParser <* char '\n')

instance Read Camel where
  readsPrec _ = readP_to_S camelParser

main :: IO ()
main = do
  sortedHBs <- sort . getHandBids . read <$> getContents
  print $ sum . zipWith ((. bid) . (*)) [1..] $ sortedHBs
