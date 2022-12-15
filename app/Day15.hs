{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-15.txt"

  putStrLn "Part 1:"
  print input

-- * Part 1

type Point = (Int, Int)

data SensorBeaconPair = SensorBeaconPair {sensor :: !Point, beacon :: !Point}
  deriving (Show)

-- | Add a position and a vector. Or two positions. I don't care.
addPos :: Point -> Point -> Point
addPos (x, y) (dx, dy) = (x + dx, y + dy)

-- | Subtract two positions.
subPos :: Point -> Point -> Point
subPos (xl, yl) (xr, yr) = (xl - xr, yl - yr)

-- ** Parsing

type Parser = Parsec Void String

parse :: String -> [SensorBeaconPair]
parse = fromRight' . Parsec.parse (sepEndBy pSensorBeaconPair eol <* eof) "input"
  where
    fromRight' (Right a) = a
    fromRight' (Left e) = error $ "That's not quite...right ;))))) " <> errorBundlePretty e

pInt :: Parser Int
pInt = read <$> some (digitChar <|> char '-')

pPoint :: Parser Point
pPoint = (,) <$> (string "x=" *> pInt <* string ", ") <*> (string "y=" *> pInt)

pSensorBeaconPair :: Parser SensorBeaconPair
pSensorBeaconPair = SensorBeaconPair <$> (string "Sensor at " *> pPoint) <*> (string ": closest beacon is at " *> pPoint)
