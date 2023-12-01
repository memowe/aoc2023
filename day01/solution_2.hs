module Main where

import Data.Char
import Data.List
import Data.Maybe
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP
import Data.Functor
import Control.Applicative

-- A class for all our number encapsulating types

class HasInt a where
  intVal :: a -> Int

-- Single digits, read as digits

data Digit  = One | Two | Three | Four | Five
            | Six | Seven | Eight | Nine
            deriving (Show, Enum, Bounded)

instance HasInt Digit where
  intVal = succ . fromEnum

parseDigit :: ReadP Digit
parseDigit = choice digitParsers
  where digitParsers  = parse <$> [minBound .. maxBound]
        parse d       = string (toLower <$> show d) >> return d

instance Read Digit where
  readsPrec _ = readP_to_S parseDigit

-- Single digits, read from digits or strings

newtype StrDigit  = StrDigit {getDigit :: Digit}
                  deriving Show

parseStrDigit :: ReadP StrDigit
parseStrDigit = intDigit <|> strDigit
  where intDigit =  satisfy (`elem` ['1'..'9'])
                <&> StrDigit . toEnum . pred . read . return
        strDigit = StrDigit <$> parseDigit

instance Read StrDigit where
  readsPrec _ = readP_to_S parseStrDigit

instance HasInt StrDigit where
  intVal = intVal . getDigit

-- Single digits, read from digits or strings, backwards

newtype RevDigit  = Rev {getStrDigit :: StrDigit}
                  deriving Show

instance Read RevDigit where
  readsPrec _ = readP_to_S (Rev <$> parseStrDigit) . reverse

instance HasInt RevDigit where
  intVal = intVal . getStrDigit

-- Parsing anything Read'able, ignoring garbage at the beginning

readMaybeWithGarbage :: Read a => String -> Maybe a
readMaybeWithGarbage cs =
  let subStrs     = concatMap inits (tails cs)
      candidates  = filter ((<= maxLength) . length) subStrs
  in  listToMaybe $ mapMaybe readMaybe candidates
  where maxLength     = maximum $ length . show <$> allStrDigits
        allStrDigits  = [minBound .. maxBound] :: [Digit]

readMaybeWithGarbageRev :: Read a => String -> Maybe a
readMaybeWithGarbageRev = readMaybeWithGarbage . reverse

-- Digit extraction

extractTwoDigitNumber :: String -> Maybe Int
extractTwoDigitNumber cs = do
  firstDigit  <- readMaybeWithGarbage cs    :: Maybe StrDigit
  lastDigit   <- readMaybeWithGarbageRev cs :: Maybe RevDigit
  return $ 10 * intVal firstDigit + intVal lastDigit

-- Run

process :: String -> String
process = show . sum . mapMaybe extractTwoDigitNumber . lines

main :: IO ()
main = interact process
