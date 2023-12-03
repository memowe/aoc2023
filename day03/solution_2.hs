module Main where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Lens.Micro.Platform

data EngineCell = Part {partNum :: Int, partLen :: Int}
                | Symbol
                | Empty
                  deriving (Eq, Ord)

newtype Engine  = Engine {cells :: [[EngineCell]]}

instance Read Engine where
 readsPrec _ = readP_to_S engine
  where engine  = Engine <$> row `endBy` char '\n'
        row     = many $ choice [part, empty, symbol]
        part    = do  num <- read <$> munch1 isDigit
                      return $ Part num (length $ show num)
        empty   = Empty <$ char '.'
        symbol  = Symbol <$ satisfy (`notElem` "0123456789.\n")

type Col            = Int
type Row            = Int
type ColRange       = [Row]
type Coord          = (Col, Row)
type ColRangeCoord  = (ColRange, Row)

withCols :: [EngineCell] -> [(EngineCell, ColRange)]
withCols = fst . foldl expand ([], 0)
  where expand (ecs, i) p@(Part _ l)  = let j = i+l-1 in (ecs++[(p,[i..j])],j+1)
        expand (ecs, i) cell          = (ecs ++ [(cell,[i])],i+1)

withCoords :: [[EngineCell]] -> [[(EngineCell, ColRangeCoord)]]
withCoords = zipWith ((. withCols) . map . (_2 %~) . flip (,)) [0..]

cellMap :: [(EngineCell, ColRangeCoord)] -> M.Map Coord (EngineCell, ColRangeCoord)
cellMap = M.fromList . concatMap expand
  where expand cell@(_, coord)  = map (,cell) (expandRange coord)
        expandRange (cs, r)     = [(c,r) | c <- cs]

neighborCoords :: Coord -> [Coord]
neighborCoords (c,r) = [(c',r') | c' <- [c-1..c+1], r' <- [r-1..r+1]]

neighbors :: Ord a => M.Map Coord a -> Coord -> [a]
neighbors m = nub . sort . mapMaybe (`M.lookup` m) . neighborCoords

gearRatios :: Engine -> [Int]
gearRatios engine =
  let coordCells    = concat $ withCoords (cells engine)
      engineMap     = cellMap coordCells
      partMap       = M.filter (isPart . fst) engineMap
      smbCoords     = M.keys $ M.filter ((== Symbol) . fst) engineMap
      neighborParts = map (neighbors partMap) smbCoords
      neighborPairs = filter ((== 2).length) neighborParts
  in  map (product . map (partNum . fst)) neighborPairs
  where isPart (Part _ _)   = True
        isPart _            = False

main :: IO ()
main = interact $ show . sum . gearRatios . read
