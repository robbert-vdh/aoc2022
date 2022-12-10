{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Ord
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-9.txt"

  putStrLn "Part 1:"
  let !p1Result = execState (execInstructions input) (initialState 2)
  print $! S.size (visitedTailPositions p1Result)

  putStrLn "\nPart 2:"
  let !p2Result = execState (execInstructions input) (initialState 10)
  print $! S.size (visitedTailPositions p2Result)

-- * Part 1/2

-- | The positions for a single state. The y-axis goes from bottom to top, so
-- moving one space up increases the y-coordinate by one. The first knot is the
-- head, and the last knot is the tail.
data RopeState = RopeState
  { knotPositions :: !(Vector (Int, Int))
  , visitedTailPositions :: HashSet (Int, Int)
  -- ^ We'll immediately keep track of which positions the tail has visited. The
  -- tail position is the very last knot.
  }
  deriving (Show)

-- Left and Right are 'Either' constructors
data Direction = DUp | DDown | DLeft | DRight deriving (Show)

-- I'd rather just unroll this but I can already feel part 2 becoming "now the
-- head moves this many spaces at once" - this didn't happen, it was worse
data Instruction = Move Int Direction deriving (Show)

-- | The initial state for a given rope length.
initialState :: Int -> RopeState
initialState n = RopeState (V.replicate n (0, 0)) (S.singleton (0, 0))

-- | Execute a list of instructions from start to finish.
execInstructions :: [Instruction] -> State RopeState ()
execInstructions = mapM_ execInstruction

-- Execute a single instruction. Which contains multiple movement steps that are
-- executed with the tail/movement in lockstep.
execInstruction :: Instruction -> State RopeState ()
execInstruction (Move n dir) = replicateM_ n $ do
  -- First the head moves, then all other knots move in order.
  moveHead dir

  -- The knots will move at most one space (in the eight directions) at a time until
  -- the  and the tail are next to each other (including diagonals, and the
  -- same space also counts). All visited positions are recorded.
  numKnots <- gets (V.length . knotPositions)
  -- Vectors and thus knots are zero-indexed
  mapM_ moveKnot [1 .. numKnots - 1]

-- | Move the head in the specified direction. Does not move the tail.
moveHead :: Direction -> State RopeState ()
moveHead DUp = addKnotPos 0 (0, 1)
moveHead DDown = addKnotPos 0 (0, -1)
moveHead DLeft = addKnotPos 0 (-1, 0)
moveHead DRight = addKnotPos 0 (1, 0)

-- | Keep moving the knot with the given index until it is at most one square
-- away from the next knot (towards the head). If the head and tail aren't in
-- the same column, then the tail will prefer moving diagonally. All positions
-- visited by the tail (the last knot) are recorded in the state. @n@ needs to
-- be in the range @[1..numKnots]@ or this will diverge.
moveKnot :: Int -> State RopeState ()
moveKnot n = do
  RopeState {knotPositions, visitedTailPositions} <- get

  let knotPos = knotPositions V.! n
      targetKnotPos = knotPositions V.! (n - 1)
      delta@(dx, dy) = targetKnotPos `subPos` knotPos
      -- The knot only ever moves a single space a ta time in of the *eight*
      -- directions
      clampedDelta = (clamp (-1, 1) dx, clamp (-1, 1) dy)

  -- Don't do anything if the knot is right next to the next knot (counting
  -- diagonals) or on the same space
  if max (abs dx) (abs dy) <= 1
    then return ()
    else do
      addKnotPos n clampedDelta

      -- Only the last knot should have its position recorded
      when (n == V.length knotPositions - 1) $
        modify' $ \s ->
          s {visitedTailPositions = S.insert (knotPos `addPos` clampedDelta) visitedTailPositions}

      -- If after moving the knot is more than one space away from the next
      -- knot, then it was possible to move more than one space before. In that
      -- case the actual delta will cover a larger distance than our clamped
      -- delta and we should recurse until the head and the tail are right next
      -- to each other.
      when (delta /= clampedDelta) (moveKnot n)

-- | Add a position offset to the nth knot in a rope.
addKnotPos :: Int -> (Int, Int) -> State RopeState ()
addKnotPos n delta =
  modify' $ \s ->
    let oldPos = knotPositions s V.! n
        newPos = oldPos `addPos` delta
     in s {knotPositions = knotPositions s V.// [(n, newPos)]}

-- | Add a position and a vector. Or two positions. I don't care.
addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (x, y) (dx, dy) = (x + dx, y + dy)

-- | Subtract two positions.
subPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
subPos (xl, yl) (xr, yr) = (xl - xr, yl - yr)

-- ** Parsing

parse :: String -> [Instruction]
parse = map pLine . lines
  where
    pLine ('U' : ' ' : s) = Move (read s) DUp
    pLine ('D' : ' ' : s) = Move (read s) DDown
    pLine ('L' : ' ' : s) = Move (read s) DLeft
    pLine ('R' : ' ' : s) = Move (read s) DRight
    pLine s = error $ "Invalid input: " <> s
