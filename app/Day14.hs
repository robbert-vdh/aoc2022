{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.List.Split (chunksOf)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-14.txt"

  putStrLn "Part 1:"
  print input

-- * Part 1

type Point = (Int, Int)

-- | A dense map of the cave. Our input is only 99 by 147 cells wide, so we
-- don't need to do anything clever here. The Y coordinates go from top to
-- bottom, so (400, 0) is above (400, 10). The bottom right corner is exclusive,
-- so if both corners have the same value then the cave is empty.
data Cave = Cave {topLeft :: Point, bottomRight :: Point, caveVector :: Vector Tile}

data Tile = Air | Rock | Sand

instance Show Cave where
  show cave@(Cave _ _ xs) = unlines . chunksOf (width cave) . map fromTile . V.toList $ xs
    where
      fromTile :: Tile -> Char
      fromTile Air = '.'
      fromTile Rock = '#'
      fromTile Sand = '+'

sandStartPos :: Point
sandStartPos = (500, 0)

-- ** Cave matrix combinators

-- | Add a position and a vector. Or two positions. I don't care.
addPos :: Point -> Point -> Point
addPos (x, y) (dx, dy) = (x + dx, y + dy)

-- | Subtract two positions.
subPos :: Point -> Point -> Point
subPos (xl, yl) (xr, yr) = (xl - xr, yl - yr)

width :: Cave -> Int
width (Cave (x1, _) (x2, _) _) = x2 - x1

height :: Cave -> Int
height (Cave (_, y1) (_, y2) _) = y2 - y1

-- | Access for a single element. Diverges when the index is out of bounds.
infixl 4 !

(!) :: Cave -> Point -> Tile
cave@(Cave _ _ xs) ! pos = xs V.! pointToLinear cave pos

-- | Unchecked access for a single element.
(//) :: Cave -> [(Point, Tile)] -> Cave
cave@(Cave tl br xs) // ops = Cave tl br $ xs V.// map (\(pos, tile) -> (pointToLinear cave pos, tile)) ops

-- The point coordinates are in the form of @(500, 0)@, so we'll need to
-- translate them according to the cave's top left coordinate.
linearToPoint :: Cave -> Int -> Point
linearToPoint cave n | w <- width cave = (n `rem` w, n `div` w) `addPos` topLeft cave
pointToLinear :: Cave -> Point -> Int
pointToLinear cave pos | w <- width cave, (x, y) <- pos `subPos` topLeft cave = x + (y * w)

-- ** Parsing

type Parser = Parsec Void String

type Line = [Point]

parse :: String -> Cave
parse = caveFromLines . fromRight' . Parsec.parse (sepEndBy pLine eol <* eof) "input"
  where
    fromRight' (Right a) = a
    fromRight' (Left e) = error $ "That's not quite...right ;))))) " <> errorBundlePretty e

caveFromLines :: [Line] -> Cave
caveFromLines [] = Cave (500, 500) (500, 500) V.empty
caveFromLines xs = foldl' addLines emptyCave xs
  where
    -- NOTE: This always needs to include (500, 0), so we'll just make sure it does
    topLeft', bottomRight' :: Point
    (topLeft', bottomRight') = foldl' minMaxPos ((maxBound, maxBound), (minBound, minBound)) (sandStartPos : concat xs)
    -- Note the @+ 1@ here, the bottom right end point is exclusive
    minMaxPos :: (Point, Point) -> Point -> (Point, Point)
    minMaxPos ((xmin, ymin), (xmax, ymax)) (x, y) = ((min xmin x, min ymin y), (max xmax (x + 1), max ymax (y + 1)))

    w = fst bottomRight' - fst topLeft'
    h = snd bottomRight' - snd topLeft'

    emptyCave :: Cave
    emptyCave = Cave topLeft' bottomRight' $ V.replicate (w * h) Air

    addLines :: Cave -> Line -> Cave
    addLines cave [] = cave
    addLines cave points = foldl' addLineSegment cave $ zip points (tail points)

    addLineSegment :: Cave -> (Point, Point) -> Cave
    addLineSegment cave ((x1, y1), (x2, y2)) =
      let xMin = min x1 x2
          xMax = max x1 x2
          yMin = min y1 y2
          yMax = max y1 y2
          lineCoords =
            if xMin == xMax
              then [(xMin, y) | y <- [yMin .. yMax]]
              else
                if yMin == yMax
                  then [(x, yMin) | x <- [xMin .. xMax]]
                  else error $ "Invalid line from " <> show (x1, y1) <> " to " <> show (x2, y2)
       in cave // map (,Rock) lineCoords

pInt :: Parser Int
pInt = read <$> some digitChar

pLine :: Parser Line
pLine = sepBy1 pPoint (string " -> ")

pPoint :: Parser Point
pPoint = (,) <$> (pInt <* char ',') <*> pInt
