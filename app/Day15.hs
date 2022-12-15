{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-15.txt"

  putStrLn "Part 1:"
  print $! sum . map rangeDistance . mergeRanges . mapMaybe (scanRangeCoveringYCoord targetYCoord) $ input

-- * Part 1

type Point = (Int, Int)

data SensorBeaconPair = SensorBeaconPair {sensor :: !Point, beacon :: !Point}
  deriving (Show)

-- | The y-coordinate at which we want to know how many tiles were searched by
-- the sensors.
targetYCoord :: Int
targetYCoord = 2000000

-- | Merge a list of potentially overlapping ranges into a list of
-- non-overlapping disjoint ranges. If all ranges overlap then the list will
-- contain only a single element.
mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges = go . sort
  where
    -- The range list is already sorted on the start index followed by the end
    -- index, so we can just merge them now
    go [] = []
    go [x] = [x]
    go (r1@(startL, endL) : r2@(startR, endR) : rs)
      | startR <= endL = go ((startL, max endL endR) : rs)
      | otherwise = r1 : go (r2 : rs)

-- | The distance covered by a range.
rangeDistance :: (Int, Int) -> Int
rangeDistance (start, end) = abs (end - start)

-- | The Manhattan distance scanned by a sensor. Because a sensor finds the
-- closest beacon, this means that within this distance, there are no other
-- beacons.
scanningDistance :: SensorBeaconPair -> Int
scanningDistance (SensorBeaconPair (xS, yS) (xB, yB)) = abs (xB - xS) + abs (yB - yS)

-- | For the given Y-coordinate, compute the range of the scan that would
-- overlap with that row. Returns @Nothing@ if the scanner is not in range of
-- that coordinate. Both end point coordinates are inclusive.
scanRangeCoveringYCoord :: Int -> SensorBeaconPair -> Maybe (Int, Int)
scanRangeCoveringYCoord targetY sbp@(SensorBeaconPair (xS, yS) _) =
  if scanWidth <= 0
    then Nothing
    else Just (xS - scanRadius, xS + scanRadius)
  where
    range = scanningDistance sbp
    deltaY = abs (targetY - yS)
    -- If @targetY@ is the same as @yS@, then the scanning width is two times
    -- the range (since scanning happens on both sides) plus 1 to account for
    -- the space the scanner occupies. For every row above or below this the
    -- width decreases by two since the scan happens in a diamond shape.
    scanWidth = ((range - deltaY) * 2) + 1
    -- This is redundant and we didn't have to compute scanWidth, but this is
    -- easier to reason about and performance isn't important here
    scanRadius = scanWidth `div` 2

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
