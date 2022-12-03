{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Char (ord)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-3.txt"

  putStrLn "Part 1:"
  print $! sum . map (sum . duplicates) $ input

  putStrLn "\nPart 2:"
  print $! sum . map (sum . duplicates') . chunksOf 3 $ input

-- | Characters are converted to integers such that a is 1, z is 26, A is 27,
-- and Z is 52.
data Rucksack = Rucksack IntSet IntSet deriving (Show)

-- * Part 1

duplicates :: Rucksack -> [Int]
duplicates (Rucksack l r) = S.toList (S.intersection l r)

-- | The priority as described in the puzzle. a is 1, z is 26, A is 27, and Z is
-- 52.
priority :: Char -> Maybe Int
priority c
  | 'a' <= c && c <= 'z' = Just $ ord c - ord 'a' + 1
  | 'A' <= c && c <= 'Z' = Just $ ord c - ord 'A' + 27
  | otherwise = Nothing

parse :: String -> [Rucksack]
parse = map pRucksack . lines
  where
    pRucksack line =
      let n = length line `div` 2
       in Rucksack
            (S.fromList . map (fromJust . priority) $ take n line)
            (S.fromList . map (fromJust . priority) $ drop n line)

-- * Part 2

-- | The same as 'duplicates', but for n entire rucksacks and without
-- distinguishing between compartments.
duplicates' :: [Rucksack] -> [Int]
duplicates' xs = S.toList (foldl1' S.intersection $ map mergeRucksack xs)
  where
    mergeRucksack (Rucksack l r) = S.union l r
