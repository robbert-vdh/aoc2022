{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.List.Split (chunksOf)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void
import GHC.Stack (HasCallStack)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-14.txt"

  putStrLn "Part 1:"
  print $! part1 $! caveFromLines input

  putStrLn "Part 2:"
  print $! part2 $! caveFromLines (addFloorLine input)

-- * Part 1

type Point = (Int, Int)

-- | A dense map of the cave. Our input is only 99 by 147 cells wide, so we
-- don't need to do anything clever here. The Y coordinates go from top to
-- bottom, so (400, 0) is above (400, 10). The bottom right corner is exclusive,
-- so if both corners have the same value then the cave is empty.
data Cave = Cave {topLeft :: Point, bottomRight :: Point, caveVector :: Vector Tile}

data Tile = Air | Rock | Sand deriving (Eq)

instance Show Cave where
  show cave@(Cave _ _ xs) = unlines . chunksOf (width cave) . map fromTile . V.toList $ xs
    where
      fromTile :: Tile -> Char
      fromTile Air = '.'
      fromTile Rock = '#'
      fromTile Sand = '+'

sandStartPos :: Point
sandStartPos = (500, 0)

-- | Count the number of sand particles that can be dropped into the cave before
-- a particle falls into the abyss.
part1 :: Cave -> Int
part1 cave = go cave 0
  where
    go :: Cave -> Int -> Int
    go cave' n
      | Just !cave'' <- dropSand cave' = go cave'' (n + 1)
      | otherwise = n

-- | Drop a single unit of sand into the cave from 'sandStartPos' Sand falls
-- down, flows downward left when downwards is blocked, flows downward right
-- when downward left is blocked, and comes to a standstill when all three tiles
-- below the sand is blocked. Returns @Nothing@ when the sand would fall into
-- the abyss below the cave.
dropSand :: Cave -> Maybe Cave
dropSand cave = (\pos -> cave // [(pos, Sand)]) <$> simulateSand cave sandStartPos

-- | Determine where a sand particle comes to rest when dropped from a certain
-- point. Returns 'Nothing' if it falls out of bounds into the abyss.
simulateSand :: Cave -> Point -> Maybe Point
simulateSand cave pos@(x, y)
  -- If the next position is out of bounds then the sand will have fallen into the
  -- abyss
  | isOutOfBounds cave (x, y + 1) = Nothing
  -- This could be much more efficient instead of simulating every single step,
  -- but this looks nice and it's fast enough anyways
  | (cave ! (x, y + 1)) == Air = simulateSand cave (x, y + 1)
  | isOutOfBounds cave (x - 1, y + 1) = Nothing
  | (cave ! (x - 1, y + 1)) == Air = simulateSand cave (x - 1, y + 1)
  | isOutOfBounds cave (x + 1, y + 1) = Nothing
  | (cave ! (x + 1, y + 1)) == Air = simulateSand cave (x + 1, y + 1)
  -- If all three places below the sand particle are solid then the sand comes
  -- to a standstill
  | otherwise = Just pos

-- ** Cave matrix combinators

-- | Add a position and a vector. Or two positions. I don't care.
addPos :: Point -> Point -> Point
addPos (x, y) (dx, dy) = (x + dx, y + dy)

-- | Subtract two positions.
subPos :: Point -> Point -> Point
subPos (xl, yl) (xr, yr) = (xl - xr, yl - yr)

-- | The width of the cave.
width :: Cave -> Int
width (Cave (x1, _) (x2, _) _) = x2 - x1

-- | The height of the cave.
height :: Cave -> Int
height (Cave (_, y1) (_, y2) _) = y2 - y1

-- | Whether or not a point would be out of bounds.
isOutOfBounds :: Cave -> Point -> Bool
isOutOfBounds (Cave (x1, y1) (x2, y2) _) (x, y) = x < x1 || y < y1 || x >= x2 || y >= y2

-- | Access for a single element. Diverges when the index is out of bounds.
infixl 4 !

(!) :: HasCallStack => Cave -> Point -> Tile
cave@(Cave _ _ xs) ! pos = xs V.! pointToLinear cave pos

-- | Unchecked access for a single element.
(//) :: HasCallStack => Cave -> [(Point, Tile)] -> Cave
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

parse :: String -> [Line]
parse = fromRight' . Parsec.parse (sepEndBy pLine eol <* eof) "input"
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

-- Note the @+ 1@ here, the bottom right end point is exclusive
minMaxPos :: (Point, Point) -> Point -> (Point, Point)
minMaxPos ((xmin, ymin), (xmax, ymax)) (x, y) = ((min xmin x, min ymin y), (max xmax (x + 1), max ymax (y + 1)))

pInt :: Parser Int
pInt = read <$> some digitChar

pLine :: Parser Line
pLine = sepBy1 pPoint (string " -> ")

pPoint :: Parser Point
pPoint = (,) <$> (pInt <* char ',') <*> pInt

-- * Part 2

-- | Hack in a floor line so we don't have to modify much else
addFloorLine :: [Line] -> [Line]
addFloorLine xs = [(xMin, y), (xMax, y)] : xs
  where
    -- The bottom right point is an exclusive end, so the coordinates are one
    -- higher than the actual bottom right corner of the cave
    topLeft', bottomRight' :: Point
    (topLeft', bottomRight') = foldl' minMaxPos ((maxBound, maxBound), (minBound, minBound)) (sandStartPos : concat xs)

    xMin = fst topLeft' - 80
    xMax = fst bottomRight' + 169

    -- This should be the highest y-coordinate plus two, so that's the bottom
    -- right coordinate plus one
    y = snd bottomRight' + 1

-- | Count the number of sand particles that can be dropped into the cave before
-- a particle overlaps the start position.
part2 :: Cave -> Int
part2 cave = go cave 0
  where
    go :: Cave -> Int -> Int
    go cave' n
      | (cave' ! sandStartPos) == Sand = n
      | Just !cave'' <- dropSand cave' = go cave'' (n + 1)
      | otherwise = error $ "The floor wasn't wide enough, hack in additional width in 'addFloorLine':\n" <> show cave'
