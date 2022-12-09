{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Ord

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-9.txt"

  putStrLn "Part 1:"
  let !result = execState (execInstructions input) initialState
  print $! S.size (visitedTailPos result)

-- * Part 1

-- | The positions for a single state. The y-axis goes from bottom to top, so
-- moving one space up increases the y-coordinate by one.
data RopeState = RopeState
  { headPos :: !(Int, Int)
  , tailPos :: !(Int, Int)
  , visitedTailPos :: HashSet (Int, Int)
  -- ^ We'll immediately keep track of which positions the tail has visited.
  }
  deriving (Show)

-- Left and Right are 'Either' constructors
data Direction = DUp | DDown | DLeft | DRight deriving (Show)

-- I'd rather just unroll this but I can already feel part 2 becoming "now the
-- head moves this many spaces at once"
data Instruction = Move Int Direction deriving (Show)

initialState :: RopeState
initialState = RopeState (0, 0) (0, 0) (S.singleton (0, 0))

-- Execute a list of instructions from start to finish.
execInstructions :: [Instruction] -> State RopeState ()
execInstructions = mapM_ execInstruction

-- Execute a single instruction.
execInstruction :: Instruction -> State RopeState ()
execInstruction (Move n dir) = do
  -- Here is that unrolling again. We now need to apply the moveHead function
  -- multiple times.
  replicateM_ n (moveHead dir)
  -- This will move at most one space (in the eight directions) at a time until
  -- the head and the tail are next to each other (including diagonals, and the
  -- same space also counts). All visited positions are recorded.
  moveTail

-- | Move the head in the specified direction. Does not move the tail.
moveHead :: Direction -> State RopeState ()
moveHead DUp = modify' $ \s -> s {headPos = headPos s `addPos` (0, 1)}
moveHead DDown = modify' $ \s -> s {headPos = headPos s `addPos` (0, -1)}
moveHead DLeft = modify' $ \s -> s {headPos = headPos s `addPos` (-1, 0)}
moveHead DRight = modify' $ \s -> s {headPos = headPos s `addPos` (1, 0)}

-- | Keep moving the tail until it is at most one square away from the head. If
-- the head and tail aren't in the same column, then the tail will prefer moving
-- diagonally. All positions visited by the tail are recorded in the state.
moveTail :: State RopeState ()
moveTail = do
  RopeState {headPos, tailPos, visitedTailPos} <- get

  let delta@(dx, dy) = headPos `subPos` tailPos
      -- The tail only ever moves a single space a ta time in of the *eight*
      -- directions
      clampedDelta = (clamp (-1, 1) dx, clamp (-1, 1) dy)
      newTailPos = tailPos `addPos` clampedDelta

  -- Don't do anything if the tail is right next to the head (counting
  -- diagonals) or on the same space
  if max (abs dx) (abs dy) <= 1
    then return ()
    else do
      modify' $ \s ->
        s
          { tailPos = newTailPos
          , visitedTailPos = S.insert newTailPos visitedTailPos
          }

      -- If after moving the tail is more than one space away from the head,
      -- then it was possible to move more than one space before. In that case
      -- the actual delta will cover a larger distance than our clamped delta
      -- and we should recurse until the head and the tail are right next to
      -- each other.
      when (delta /= clampedDelta) moveTail

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
