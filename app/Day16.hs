{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-16.txt"

  putStrLn "Part 1:"
  -- print $! maximumPressureReleaseDfs input
  print $! computeDistanceMatrix input

-- * Part 1

type ValveKey = String

-- This is the bare cyclic undirected graph you'd get from parsing the input
data Valve = Valve {flowRate :: {-# UNPACK #-} !Int, connectedTo :: [ValveKey]} deriving (Show)
type Graph = HashMap ValveKey Valve

-- The distance between two pairs of valves. Valves with flow rate 0 are omitted
-- here.
type DistanceMatrix = HashMap (ValveKey, ValveKey) Int
type MaybeDistanceMatrix = HashMap (ValveKey, ValveKey) (Maybe Int)

-- data SearchState = SearchState
--   { currentPos :: !ValveKey
--   -- ^ The current position in the graph. Starts at AA.
--   , timeRemaining :: {-# UNPACK #-} !Int
--   -- ^ How many time units are remaining. Starts at 30.
--   , openValves :: !(HashSet ValveKey)
--   -- ^ Which valves are already open.
--   , resultingPressureRelease :: {-# UNPACK #-} !Int
--   -- ^ How much pressure would be released at the end of the 30 minutes. Starts
--   -- at 0.
--   }
--   deriving (Show)

-- initialSearchState :: SearchState
-- initialSearchState = SearchState "AA" 30 S.empty 0

-- | Compute the distance matrix from a graph. Valves with flow rate 0 are
-- omitted except for valve AA, and connections from a node to itself is also
-- filtered out. This is simply an all-pair shortest path implementation using
-- the Floyd-Warshall algorithm.
computeDistanceMatrix :: Graph -> DistanceMatrix
computeDistanceMatrix graph = pruneZeroFlow (computeDistanceMatrix' graph)
  where
    pruneZeroFlow = M.filterWithKey $ \(x, y) _ ->
      x == "AA"
        || (flowRate (graph M.! x) > 0 && flowRate (graph M.! y) > 0)
        || (x == y)

-- | 'computeDistanceMatrix', but without pruning uninteresting vertices.
computeDistanceMatrix' :: Graph -> DistanceMatrix
computeDistanceMatrix' graph =
  M.map (fromMaybe maxBound) . foldThrough $
    -- Note the order here, this is folded from right to left
    M.unions [directEdges, zeroDistances, initMatrix]
  where
    keys :: [ValveKey]
    keys = M.keys graph

    initMatrix, zeroDistances, directEdges :: MaybeDistanceMatrix
    -- The distance matrix is initialized with infinite distances.
    initMatrix = M.fromList [((x, y), Nothing) | x <- keys, y <- keys]
    -- The distance from a node to itself is 0.
    zeroDistances = M.fromList $ map (\k -> ((k, k), Just 0)) keys
    -- And the direct edges should be added to the graph.
    directEdges = M.fromList $ concatMap (\x -> map (\y -> ((x, y), Just 1)) (connectedTo $ graph M.! x)) keys

    -- These are the three nested loops of the Floyd-Warshall algorithm. If
    -- @|(x, k)| + |(k, y)| < (x, y)|@, then that distance will be used instead.
    -- We'll name @x@ @from@, @k@ @through@, and @y@ @to@ to make this a bit
    -- clearer.
    foldThrough :: MaybeDistanceMatrix -> MaybeDistanceMatrix
    foldThrough matrix = foldl' (\matrix' through -> foldFrom matrix' through) matrix keys

    foldFrom :: MaybeDistanceMatrix -> ValveKey -> MaybeDistanceMatrix
    foldFrom matrix through = foldl' (\matrix' from -> foldTo matrix' through from) matrix keys

    foldTo :: MaybeDistanceMatrix -> ValveKey -> ValveKey -> MaybeDistanceMatrix
    foldTo matrix through from = foldl' (\matrix' to -> maybeUpdateDistance matrix' from through to) matrix keys

    maybeUpdateDistance :: MaybeDistanceMatrix -> ValveKey -> ValveKey -> ValveKey -> MaybeDistanceMatrix
    maybeUpdateDistance matrix from through to =
      let directDistance = matrix M.! (from, to)
          throughDistance = liftM2 (+) (matrix M.! (from, through)) (matrix M.! (through, to))
       in case (directDistance, throughDistance) of
            -- Take the distance through @through@ if it is smaller than the
            -- current distance (or if is the first possible path from @from@ to
            -- @to)
            (Nothing, Just dThrough) -> M.insert (from, to) (Just dThrough) matrix
            (Just dDirect, Just dThrough)
              | dThrough < dDirect ->
                  M.insert (from, to) (Just dThrough) matrix
            _ -> matrix

-- -- The distance from a node to itself is 0.
-- zeroDistances = M.fromList . map (\k -> ((k, k), 0)) $ M.keys graph

-- -- | We'll start simple and just solve this with a depth first search.
-- maximumPressureReleaseDfs :: Graph -> Int
-- maximumPressureReleaseDfs graph = maximumPressureReleaseDfs' graph initialSearchState

-- -- | Find the maximum pressure release possible from a given starting search
-- -- state using a depth first search.
-- maximumPressureReleaseDfs' :: Graph -> SearchState -> Int
-- maximumPressureReleaseDfs' graph ss = case possibleActions graph ss of
--   -- If there are no actions remaining then the time is up
--   [] -> resultingPressureRelease ss
--   actions ->
--     let ssResults = map (performAction graph ss) actions
--      in maximum $! map (maximumPressureReleaseDfs' graph) ssResults

-- data Action = OpenCurrentValve | MoveTo ValveKey

-- -- | Get the possible actions based on the current position, open valves, and
-- -- time remaining. Returns an empty list if there is no time left to perform an
-- -- action.
-- possibleActions :: Graph -> SearchState -> [Action]
-- possibleActions graph ss
--   -- Both actions take exactly one minute
--   | timeRemaining ss < 1 = []
--   | otherwise =
--       let currentValveKey = currentPos ss
--           currentValve = graph M.! currentValveKey
--           moveActions = map MoveTo (connectedTo $ graph M.! currentValveKey)
--        in -- We can of course only open the current valve if it is not already
--           -- open. We'll also preemptively prune blocked valves because why not.
--           if currentValveKey `S.member` openValves ss || flowRate currentValve == 0
--             then moveActions
--             else OpenCurrentValve : moveActions

-- -- | Apply the effects of an action returned by 'possibleActions'. This does not
-- -- check whether the action actually makes any sense.
-- performAction :: Graph -> SearchState -> Action -> SearchState
-- performAction graph ss OpenCurrentValve =
--   let currentValveKey = currentPos ss
--       currentValve = graph M.! currentValveKey
--       timeRemaining' = timeRemaining ss - 1
--    in ss
--         { -- The valve only opens after the minute has passed, so this should use
--           -- the new flow rate
--           resultingPressureRelease = resultingPressureRelease ss + (flowRate currentValve * timeRemaining')
--         , openValves = currentValveKey `S.insert` openValves ss
--         , timeRemaining = timeRemaining'
--         }
-- performAction _ ss (MoveTo to) = ss {currentPos = to, timeRemaining = timeRemaining ss - 1}

-- ** Parsing

type Parser = Parsec Void String

parse :: String -> Graph
parse = M.fromList . fromRight' . Parsec.parse (sepEndBy pValveKV eol <* eof) "input"
  where
    fromRight' (Right a) = a
    fromRight' (Left e) = error $ "That's not quite...right ;))))) " <> errorBundlePretty e

pInt :: Parser Int
pInt = read <$> some (digitChar <|> char '-')

pValveKey :: Parser ValveKey
pValveKey = (\x y -> [x, y]) <$> upperChar <*> upperChar

pValveKV :: Parser (ValveKey, Valve)
pValveKV = (,) <$> (string "Valve " *> pValveKey) <*> pValve
  where
    pValve :: Parser Valve
    pValve =
      Valve
        <$> (string " has flow rate=" *> pInt)
        <*> ( string "; tunnel"
                -- Ugh
                *> (string "s lead to valves " <|> string " leads to valve ")
                *> sepBy1 pValveKey (string ", ")
            )
