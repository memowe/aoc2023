module Main where

import Data.Char
import Data.List
import Data.Map (Map, (!), elems, fromList, unionWith)
import Text.ParserCombinators.ReadP

data ScratchCard = SC {num :: Int, winning :: [Int], have :: [Int]}

instance Read ScratchCard where
  readsPrec _ = readP_to_S card
    where card = do
            scNum     <- string "Card" >> sp >> int
            scWinning <- char ':' >> sp >> ints
            scHave    <- sp >> char '|' >> sp >> ints
            return $ SC scNum scWinning scHave
          ints  = int `sepBy` sp
          int   = read <$> munch1 isDigit
          sp    = munch1 (== ' ')

matches :: ScratchCard -> [Int]
matches = intersect <$> winning <*> have

collectAll :: [ScratchCard] -> Map Int Int
collectAll scs = foldl inject initial scs
  where initial     = fromList $ map ((,1) . num) scs
        inject m sc = let times     = m ! num sc
                          nextCards = fromList $ (,times) <$> collect sc
                      in  unionWith (+) nextCards m
        collect sc  = take (length $ matches sc) [num sc + 1 ..]

main :: IO ()
main = do cards <- map read . lines <$> getContents
          let collection = collectAll cards
          print $ sum (elems collection)
