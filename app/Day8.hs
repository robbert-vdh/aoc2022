{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (digitToInt)
import Data.Massiv.Array (D, DL, Dim (..), Ix1, Matrix, U, Vector)
import qualified Data.Massiv.Array as A

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-8.txt"

  putStrLn "Part 1:"
  print $! A.sum . A.map fromEnum $ checkVisiblity input

-- * Part 1

type Grid a = Matrix U a

-- | Compute the entire visibility matrix
checkVisiblity :: Grid Int -> Matrix D Bool
checkVisiblity grid =
  A.zipWith
    (||)
    (A.compute @U $ checkRows grid)
    (A.compute @U $ checkCols grid)

-- | Create visibility vectors for all rows. These are checked from left to
-- right and from right to left.
checkRows :: Grid Int -> Matrix DL Bool
checkRows = mapRows bidirectionalRowVisibility

-- | Create visibility vectors for all columns. These are checked from top to
-- bottom and from bottom to top.
checkCols :: Grid Int -> Matrix DL Bool
checkCols = mapCols bidirectionalRowVisibility

mapRows :: (A.Source r a, A.Unbox a) => (Vector U Int -> Vector r a) -> Grid Int -> Matrix DL a
mapRows f = A.throwEither . A.stackOuterSlicesM . A.map f . A.outerSlices

mapCols :: (A.Source r a, A.Unbox a) => (Vector U Int -> Vector r a) -> Grid Int -> Matrix DL a
mapCols f = A.throwEither . A.stackInnerSlicesM . A.map (f . A.compute @U) . A.innerSlices

-- | Create a visibility vector out of a row (or column) slice. This only goes
-- from start to end. Use 'bidirectionalRowVisibility' to check both
-- directions.
rowVisibility :: A.Manifest r Int => Vector r Int -> Vector DL Bool
rowVisibility vec = A.iunfoldrS_ (A.size vec) go (-1)
  where
    -- I couldn't find a better convenience function to implement this scan
    -- with, so we're using this unfold to keep track of the last seen height so
    -- we can output whether the current height is larger or smaller than the
    -- last one
    go :: Int -> Ix1 -> (Bool, Int)
    go lastHeight ix =
      let currentHeight = vec A.! ix
          visible = currentHeight > lastHeight
       in (visible, max lastHeight currentHeight)

bidirectionalRowVisibility :: (A.Manifest r Int) => Vector r Int -> Vector D Bool
bidirectionalRowVisibility vec =
  A.zipWith
    (||)
    (A.compute @U . rowVisibility $ vec)
    -- Laziness is great until it isn't. We want to reverse the row, compute the
    -- visibility, and reverse the result.
    (A.reverse' (Dim 1) . A.compute @U . rowVisibility . A.compute @U . A.reverse' (Dim 1) $ vec)

-- ** Parsing

parse :: String -> Grid Int
parse = A.fromLists' A.Seq . map (map digitToInt) . lines
