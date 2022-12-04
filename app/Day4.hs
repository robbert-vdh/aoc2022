{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List.Split

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-4.txt"

  putStrLn "Part 1:"
  print $! sum . map (\(l, r) -> fromEnum (symmetricalContains l r)) $ input

  putStrLn "\nPart 2:"
  print $! sum . map (\(l, r) -> fromEnum (overlap l r)) $ input

-- * Part 1

data Range = Range Int Int deriving (Show)

-- | Check whether the first range contains the second one. This assumes the
-- second number in the range is equal to or greater than the first number.
contains :: Range -> Range -> Bool
contains (Range lx ly) (Range rx ry) = lx <= rx && ry <= ly

symmetricalContains :: Range -> Range -> Bool
symmetricalContains l r = contains l r || contains r l

parse :: String -> [(Range, Range)]
parse = map pLine . lines
  where
    pLine (splitOn "," -> [l, r]) = (pRange l, pRange r)
    pLine _ = error "Input line does not contain exactly one comma"

    pRange (splitOn "-" -> [l, r]) = Range (read l) (read r)
    pRange _ = error "Invalid input format"

-- * Part 2

overlap :: Range -> Range -> Bool
overlap (Range lx ly) (Range rx ry) = (rx <= ly && ly <= ry) || (lx <= ry && ry <= ly)
