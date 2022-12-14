{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List
import Data.Maybe
import Data.Ord
import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-16.txt"

  putStrLn "Part 1:"
  print $! maximumPressureRelease input

  putStrLn "\nPart 2: (may not yield the correct result for all inputs, see the note)"
  print $! maximumElephantPoweredPressureRelease input

-- * Part 1

type ValveKey = String

-- This is the bare cyclic undirected graph you'd get from parsing the input
data Valve = Valve {flowRate :: {-# UNPACK #-} !Int, connectedTo :: [ValveKey]} deriving (Show)
type Graph = HashMap ValveKey Valve

-- The distance between two pairs of valves. Valves with flow rate 0 are omitted
-- here.
type DistanceMatrix = HashMap (ValveKey, ValveKey) Int
type MaybeDistanceMatrix = HashMap (ValveKey, ValveKey) (Maybe Int)

-- | Find the path through the graph with the highest maximum pressure release.
-- This works by first computing the shortest paths for all pairs of nodes, only
-- considering valves with non-zero flow. Then based on that we'll find all path
-- permutations starting at @AA@ that visit and turn on all other valves in the
-- allotted time. Once we have that, we can simply take the path that results in
-- in the maximum amount of pressure released at the end of the 30 minutes.
maximumPressureRelease :: Graph -> Int
maximumPressureRelease graph = maximum $ map fst pathPermutations
  where
    matrix :: DistanceMatrix
    !matrix = computeDistanceMatrix graph

    -- We'll want to visit all of the valves at least once.
    targets :: HashSet ValveKey
    !targets = S.fromList $ map snd (M.keys matrix)

    -- These are all possible paths, along with the total amount of pressure
    -- released at the end of the 30 minutes. Since we cannot visit every
    -- possible valve with the real input, this also includes incomplete paths.
    -- We'll always start at @AA@.
    pathPermutations :: [(Int, [ValveKey])]
    pathPermutations =
      map (\(x, path) -> (x, "AA" : path)) $
        expandPathPermutations (graph, matrix, 30) "AA" targets 0

-- | The graph, distance matrix, and maximum allotted time to solve the problem.
type SolverParams = (Graph, DistanceMatrix, Int)

-- | These are all possible paths that visit all valves. This considers the
-- valves that have already been visited, and the current time. The path is
-- pruned when the time limit is up before all valves can be turned on.
--
-- This was previously defined in the where clause of 'maximumPressureRelease'
-- but we need a modified version for part 2, hence the pretty intense function
-- signature.
expandPathPermutations :: SolverParams -> ValveKey -> HashSet ValveKey -> Int -> [(Int, [ValveKey])]
expandPathPermutations params@(graph, matrix, maximumTime) from unvisited currentTime
  -- If we have visited all paths we can bubble up again and add the
  -- contributed pressure releases and the valves along the path
  | S.null unvisited = [(0, [])]
  | otherwise =
      concatMap expandPathPermutation unvisited
  where
    expandPathPermutation :: ValveKey -> [(Int, [ValveKey])]
    expandPathPermutation to =
      -- We need to travel to the new valve, and then open it which also takes
      -- one minute.
      let !newTime = currentTime + (matrix M.! (from, to)) + 1
          valve = graph M.! to
          !contributedPressureRelease = flowRate valve * (maximumTime - newTime)
          unvisited' = S.delete to unvisited
       in if newTime > maximumTime
            then -- For the example case we can reject all longer paths, but with our
            -- real input we cannot visit all valves
              [(0, [])]
            else
              map
                (\(time, path) -> (time + contributedPressureRelease, to : path))
                $! expandPathPermutations params to unvisited' newTime

-- | Compute the distance matrix from a graph. Valves with flow rate 0 are
-- omitted except for valve AA, and connections from a node to itself is also
-- filtered out. This is simply an all-pair shortest path implementation using
-- the Floyd-Warshall algorithm.
computeDistanceMatrix :: Graph -> DistanceMatrix
computeDistanceMatrix graph = pruneZeroFlow (computeDistanceMatrix' graph)
  where
    pruneZeroFlow = M.filterWithKey $ \(x, y) _ ->
      (x == "AA" && flowRate (graph M.! y) > 0)
        || (flowRate (graph M.! x) > 0 && flowRate (graph M.! y) > 0 && x /= y)

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

-- * Part 2

-- | This is the same as 'maximumElephantPoweredPressureRelease', but with a 26
-- minute time limit, and after finding a path we'll find a second path that
-- doesn't revisit any nodes and sum the scores.
maximumElephantPoweredPressureRelease :: Graph -> Int
maximumElephantPoweredPressureRelease graph = maximum $ map (\(x, _, _) -> x) pathPermutations
  where
    matrix :: DistanceMatrix
    !matrix = computeDistanceMatrix graph

    -- We'll want to visit all of the valves at least once.
    targets :: HashSet ValveKey
    !targets = S.fromList $ map snd (M.keys matrix)

    -- These are all possible combinations of paths where we and the elephant
    -- both visit different valves, along with the total amount of pressure
    -- released at the end of the 30 (or well 26) minutes. We'll always start at
    -- @AA@.
    pathPermutations :: [(Int, [ValveKey], [ValveKey])]
    pathPermutations =
      let ourPaths = expandPathPermutations (graph, matrix, 26) "AA" targets 0
          -- HACK: I couldn't be bothered to reengineer this so part 2 can be
          --       faster, so we'll just brute force it by limiting the search
          --       space to only consider the best paths
          yolo = take 10 $ sortOn (Down . fst) ourPaths
          -- The elephant will visit the nodes we haven't already visited
          augmentedPaths = concatMap (uncurry elephantPermutations) yolo
       in map (\(x, ourPath, elePhath) -> (x, "AA" : ourPath, "AA" : elePhath)) augmentedPaths

    -- Compute an addition set of paths that doesn't overlap with any of the
    -- valves from @ourPath@.
    elephantPermutations :: Int -> [ValveKey] -> [(Int, [ValveKey], [ValveKey])]
    elephantPermutations ourPressureReleased ourPath =
      -- The nodes that have already been visited by us shouldn't be visited by the elephant
      let availableTargets = S.difference targets (S.fromList ourPath)
          elePhaths = expandPathPermutations (graph, matrix, 26) "AA" availableTargets 0
       in map (\(x, elePhath) -> (ourPressureReleased + x, ourPath, elePhath)) elePhaths
