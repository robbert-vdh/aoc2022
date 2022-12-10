{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.State.Strict
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-10.txt"

  putStrLn "Part 1:"
  let !xs = evalState (run input) initialState
  print $! sum $! signalStrengths xs

-- * Part 1

data CpuState = CpuState
  { registerX :: {-# UNPACK #-} !Int
  -- NOTE: I thought we had to do pipelining. We don't.
  -- , pendingInstr :: ![PendingInstruction]
  }
  deriving (Show)

-- | Program instructions that can be executed by the CPU.
data Instruction
  = -- | After /two/ cycles (so at the end of the next cycle), add the value to
    -- the X register.
    AddX Int
  | -- | Don't do anything this cycle.
    Noop
  deriving (Show)

-- NOTE: Apparently there is no pipelining. I'll keep this around in case part 2
--       does want you to implement pipelining.
-- -- | Instructions that are still processing on the CPU's pipeline and that will
-- -- yield a result later.
-- data PendingInstruction
--   = -- | @AddXAfter n x@ means that after @n@ cycles, @x@ should be added to the
--     -- X register. @n@ starts at 2 in the cycle the 'AddX' instruction was
--     -- executed. It drops by 1 for every cycle, and the effect is executed when
--     -- @n@ becomes 0. The latency is also dropped by 1 at the end of the cycle
--     -- 'AddX' was executed on.
--     AddXAfter Int Int
--   deriving (Show)

-- | The X register starts with value 1.
initialState :: CpuState
initialState = CpuState 1

-- | Compute the signal strengths from a list of register X values.
signalStrengths :: [Int] -> [Int]
signalStrengths xs = mapMaybe strength (zip [2 ..] xs)
  where
    -- NOTE: The n is one higher than you'd expect because they're interested in
    --       the value _during the cycle_,, and /addx/ causes the value to
    --       increase at the /end/ of a cycle.
    strength (n, x)
      | n == 20 || ((n - 20) `rem` 40) == 0 = Just (n * x)
      | otherwise = Nothing

-- | Run a list of instructions. Returns the value of the X register at every step.
run :: [Instruction] -> State CpuState [Int]
run instrs = concat <$> mapM execInstr instrs

-- NOTE: I thought pipelining was a thing, it's not
-- -- | Run a list of instructions, then wait until there are no more pending
-- -- instructions. Returns the value of the X register at every step.
-- run :: [Instruction] -> State CpuState [Int]
-- run instrs = do
--   xs <- forM instrs $ \instr -> do
--     step (Just instr)
--     getX

--   -- After executing all steps, we'll keep evaluating the pending pending
--   -- instructions until none are left. This is super inefficient but we'll do
--   -- this for at most two cycles anyways.
--   hasPending <- not . null <$> gets pendingInstr
--   if hasPending
--     then (xs ++) <$> runPending
--     else return xs
--   where
--     runPending :: State CpuState [Int]
--     runPending = do
--       step Nothing
--       pendingX <- getX

--       hasPending <- not . null <$> gets pendingInstr
--       if hasPending
--         then (pendingX :) <$> runPending
--         else return [pendingX]

-- | Get the value of the X register.
getX :: State CpuState Int
getX = gets registerX

-- | Execute an instruction, yielding the value of register X at the end of
-- every cycle (which may be one or more values).
execInstr :: Instruction -> State CpuState [Int]
execInstr (AddX x) = do
  initialX <- getX
  let newX = initialX + x
  modify' $ \s -> s {registerX = newX}

  -- This operation takes two cycles, so at the end of the first cycle the
  -- register still contains the old value
  return [initialX, newX]
execInstr Noop = (: []) <$> getX

-- NOTE: This version does pipelining, which you don't have to do here
-- -- | Execute an single cycle, with or without a new instruction.
-- step :: Maybe Instruction -> State CpuState ()
-- step (Just instr) = execInstr instr >> execPending
-- step Nothing = execPending

-- -- | Execute an instruction. A cycle consists of calling this function (unless
-- -- no more instructions remain but there are still pending instructions),
-- -- followed by 'execPending'.
-- execInstr :: Instruction -> State CpuState ()
-- execInstr (AddX x) = modify' $ \s -> s {pendingInstr = AddXAfter 2 x : pendingInstr s}
-- execInstr Noop = return ()

-- -- | Handle the pending instructions. This reduces all pending instruction's
-- -- latencies by 1, and then executes the effects of the instructions with
-- -- latency 0.
-- execPending :: State CpuState ()
-- execPending = do
--   pending <- gets pendingInstr

--   -- A pending instruction may yield another pending instruction if the latency
--   -- at the start of the cycle was higher than 1, or it may have been executed
--   -- without yielding a new pending instruction
--   pending' <- catMaybes <$> mapM dewit pending
--   modify' $ \s -> s {pendingInstr = pending'}
--   where
--     -- \| Execute a single pending instruction. This should not modify the
--     -- pendingInstr field but instead return either a new pending instruction
--     -- (if the latency was merely reduced by one) or nothing.
--     dewit :: PendingInstruction -> State CpuState (Maybe PendingInstruction)
--     dewit (AddXAfter 1 x) = Nothing <$ modify' (\s -> s {registerX = registerX s + x})
--     dewit (AddXAfter n x) = pure . Just $ AddXAfter (n - 1) x

-- ** Parsing

parse :: String -> [Instruction]
parse = map (pInstr . words) . lines
  where
    pInstr :: [String] -> Instruction
    pInstr ["addx", n] = AddX (read n)
    pInstr ["noop"] = Noop
    pInstr xs = error $ "Invalid input: " <> show xs
