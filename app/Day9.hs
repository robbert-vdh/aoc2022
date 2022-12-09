{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main where

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-9.txt"

  putStrLn "Part 1:"
  print input

-- * Part 1

-- | The positions for a single state. The y-axis goes from bottom to top, so
-- moving one space up increases the y-coordinate by one.
data State = State {headPos :: !(Int, Int), tailPos :: !(Int, Int)}
  deriving (Show)

-- Left and Right are 'Either' constructors
data Direction = DUp | DDown | DLeft | DRight deriving (Show)

-- I'd rather just unroll this but I can already feel part 2 becoming "now the
-- head moves this many spaces at once"
data Instruction = Move Int Direction deriving (Show)

initialState :: State
initialState = State (0, 0) (0, 0)

-- ** Parsing

parse :: String -> [Instruction]
parse = map pLine . lines
  where
    pLine ('U' : ' ' : s) = Move (read s) DUp
    pLine ('D' : ' ' : s) = Move (read s) DDown
    pLine ('L' : ' ' : s) = Move (read s) DLeft
    pLine ('R' : ' ' : s) = Move (read s) DRight
    pLine s = error $ "Invalid input: " <> s
