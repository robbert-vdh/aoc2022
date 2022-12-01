{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List (sortOn)
import Data.Ord (Down (Down))

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-1.txt"

  putStrLn "Part 1:"
  print $! maximum (map sum input)

  putStrLn "\nPart 2:"
  print $! (sum . take 3 . sortOn Down . map sum) input

parse :: String -> [[Int]]
parse = map (map read) . splitOn "" . lines

-- | Split a list on a separator. Doesn't include the separator.
splitOn :: forall a. Eq a => a -> [a] -> [[a]]
splitOn sep = dropWhile null . foldr go []
  where
    -- This may prepend an empty list to the output, hence the @dropWhile null@.
    go :: a -> [[a]] -> [[a]]
    -- Prepend at most one empty list when encountering a separator
    go a ([] : accs) | a == sep = [] : accs
    go a accs | a == sep = [] : accs
    -- Otherwise the element should be prepended to the first accumulator list
    go a [] = [[a]]
    go a (acc : accs) = (a : acc) : accs
