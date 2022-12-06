{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (nub)

main :: IO ()
main = do
  !input <- readFile "inputs/day-6.txt"

  putStrLn "Part 1:"
  print $! packetEndPos input

  putStrLn "\nPart 1:"
  print $! packetStartPos input

-- * Part 1

-- | Return the 1-based index of the last character of the first unique sequence
-- of four letters in the input. For @abcdef@ this returns 4, and for @aaabcdf@
-- this returns 7. Returns 'Nothing' if no such sequence was found.
packetEndPos :: String -> Maybe Int
packetEndPos = go 4
  where
    go :: Int -> String -> Maybe Int
    go n (x : y : z : w : xs)
      | x /= y && x /= z && x /= w && y /= z && y /= w && z /= w =
          Just n
      | otherwise = go (n + 1) (y : z : w : xs)
    go _ _ = Nothing

-- * Part 2

-- | The same as 'packetEndPos', but for a 14 element sequence.
packetStartPos :: String -> Maybe Int
packetStartPos = go 14
  where
    -- This could be a whole load more efficient using Vectors, HashSets, and/or
    -- some simple dynamic programming, but I didn't feel like adding more
    -- libraries and for the test case this is more than fast enough.
    go :: Int -> String -> Maybe Int
    go _ [] = Nothing
    go n xs = case take 14 xs of
      seg | length seg == 14 && length (nub seg) == 14 -> Just n
      _ -> go (n + 1) (tail xs)
