{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (assert)
import Data.Char
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-12.txt"

  putStrLn "Part 1:"
  print $! Seq.length <$> findShortestPath input (fromJust $ start input) (fromJust $ goal input)

  putStrLn "\nPart 2:"
  -- If you compute a map of optimal directions from each a square this can be
  -- done much more efficiently, but I'll just brute force it
  let startCandidates = [(x, y) | x <- [0..width input - 1], y <- [0..height input - 1], (input ! (x, y)) <= 1]
      lengthFrom pos = Seq.length <$> findShortestPath input pos (fromJust $ goal input)
  print $! minimum . mapMaybe lengthFrom $! startCandidates

-- * Part 1

-- | A height map with a certain width.
--
-- NOTE: The starting position S was giving height 0 and the goal E was given
--       height 27. This ended up being a bad idea. See @normalize@.
data Heightmap = Heightmap Int (Vector Int)

instance Show Heightmap where
  show (Heightmap w xs) = unlines . chunksOf w . map fromHeight . V.toList $ xs
    where
      fromHeight :: Int -> Char
      fromHeight 0 = 'S'
      fromHeight 27 = 'E'
      fromHeight n = chr (n + ord 'a' - 1)

type Path = Seq (Int, Int)
type Expanded = HashSet (Int, Int)
type Candidates = MinPQueue Int Path

-- | Find the shortest path through a heightmap from the goal to the finish. The
-- result is a list of moves from the goal to reach the finish, if a path can be
-- found.
findShortestPath :: Heightmap -> (Int, Int) -> (Int, Int) -> Maybe Path
findShortestPath hm from to = findShortestPath' (normalize hm) to initialExpanded initialCandidates
  where
    initialExpanded = S.singleton from
    initialPaths = map Seq.singleton $ expand hm initialExpanded from
    initialCandidates = candidatesFromPaths to initialPaths

    -- NOTE: It seemed like a great idea to encode the start and position as
    --       special values like this. It wasn't. This works fine for the
    --       example, but not for my input file.
    normalize :: Heightmap -> Heightmap
    normalize (Heightmap w xs) = Heightmap w (V.map normalizeHeight xs)

    normalizeHeight :: Int -> Int
    normalizeHeight 0 = 1
    normalizeHeight 27 = 26
    normalizeHeight n = n

-- | Find a path to @to@, with the given set of already expanded places, and the
-- provided priority queue of next paths to expand.
findShortestPath' :: Heightmap -> (Int, Int) -> Expanded -> Candidates -> Maybe Path
findShortestPath' hm to !expanded !candidates
  | Just (path@(_ Seq.:|> lastPos), candidates') <- PQ.minView candidates =
      if lastPos == to
        then Just path
        else
          let newCandidates = expand hm expanded lastPos
              newPaths = map (path Seq.:|>) newCandidates
              !expanded' = S.union expanded (S.fromList newCandidates)
              !candidates'' = PQ.union candidates' (candidatesFromPaths to newPaths)
           in findShortestPath' hm to expanded' candidates''
  -- If the priority queue is empty then the search ends
  | otherwise = Nothing

-- | Compute a priority queue with path costs based on a list of paths.
candidatesFromPaths :: (Int, Int) -> [Path] -> Candidates
candidatesFromPaths to = PQ.fromList . map (\path -> (pathCost to path, path))

-- | Compute the host of a path based on its length and the Manhattan distance
-- to the goal.
pathCost :: (Int, Int) -> Path -> Int
pathCost (toX, toY) path@(_ Seq.:|> (x, y)) = Seq.length path + abs (toX - x) + abs (toY - y)
pathCost _ _ = error "Empty path"

-- | Get the next spaces to explore from a space while avoiding expanding spaces
-- that have already been expanded.
expand :: Heightmap -> Expanded -> (Int, Int) -> [(Int, Int)]
expand hm expanded pos@(x, y) =
  traceResult $
    filter
      isAdmissible
      [ (x - 1, y)
      , (x, y - 1)
      , (x + 1, y)
      , (x, y + 1)
      ]
  where
    -- traceResult result = trace (show pos <> " :: " <> show expanded <> " :: " <> show result) result
    traceResult result = result

    currentHeight = hm ! pos
    w = width hm
    h = height hm

    isAdmissible pos'@(x', y') =
      x' >= 0
        && x' < w
        && y' >= 0
        && y' < h
        -- The space should not have been expanded
        && not (S.member pos' expanded)
        -- And the height may not be more than 1 higher than the current
        -- position
        && ((hm ! pos') <= currentHeight + 1)

-- ** Map combinators and utility functions

width :: Heightmap -> Int
width (Heightmap w _) = w

height :: Heightmap -> Int
height (Heightmap w xs) = V.length xs `div` w

infixl 4 !
(!) :: Heightmap -> (Int, Int) -> Int
Heightmap w xs ! (x, y) = xs V.! (x + (y * w))

elemIndex :: Int -> Heightmap -> Maybe (Int, Int)
elemIndex f (Heightmap w xs) = (\i -> (i `rem` w, i `div` w)) <$> V.elemIndex f xs

-- ** Parsing

-- NOTE: This ended up not being a great idea, see @normalize@
start :: Heightmap -> Maybe (Int, Int)
start = elemIndex 0

goal :: Heightmap -> Maybe (Int, Int)
goal = elemIndex 27

parse :: String -> Heightmap
parse = toHeightmap . map (map toHeight) . lines
  where
    toHeight :: Char -> Int
    toHeight 'S' = 0
    toHeight 'E' = 27
    toHeight c = ord c - ord 'a' + 1

    toHeightmap :: [[Int]] -> Heightmap
    toHeightmap rows =
      let w = length (head rows)
          heightmap = Heightmap w (V.fromList $ concat rows)
       in assert
            (all (\row -> length row == w) rows)
            heightmap
