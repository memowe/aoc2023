module Main where

import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Text.ParserCombinators.ReadP

data Color =  Red | Green | Blue
              deriving (Eq, Show, Enum, Bounded, Ord)

type Handful = Map Color Int

data Game = Game {gameId :: Int, handfuls :: [Handful]}
            deriving Show

instance Read Game where
  readsPrec _ = readP_to_S game
    where game    = do  gid <- read <$> between (string "Game ") (string ": ") (munch1 isDigit)
                        hfs <- handful `sepBy1` string "; "
                        return $ Game gid hfs
          handful = M.fromList <$> cnum `sepBy` string ", "
          cnum    = do  num <- read <$> munch1 isDigit
                        col <- char ' ' >> color
                        return (col, num)
          color   = choice (pcol <$> [minBound .. maxBound])
          pcol c  = string (toLower <$> show c) >> return c

gameMinHand :: Game -> Handful
gameMinHand = M.unionsWith max . handfuls

gamePower :: Game -> Int
gamePower = product . M.elems . gameMinHand

main :: IO ()
main = interact $ \input ->
  let games = read <$> lines input
  in  show $ sum (gamePower <$> games)
