{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (digitToInt)
import Data.Massiv.Array (D, DL, Dim (..), Ix1, Matrix, Sz (..), U, Vector, pattern Ix1)
import qualified Data.Massiv.Array as A

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-8.txt"

  putStrLn "Part 1:"
  print $! A.sum . A.map fromEnum $ checkVisiblity input

  putStrLn "\nPart 2:"
  print $! A.maximum' $ mapVisibleTrees input

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

-- * Part 2

-- | Compute the entire visibility matrix
mapVisibleTrees :: Grid Int -> Matrix D Int
mapVisibleTrees grid =
  A.zipWith
    (*)
    (A.compute @U $ computeRows grid)
    (A.compute @U $ computeCols grid)

-- | Create the number of visible trees for all rows. This the product of the
-- computations from left to right and from right to left.
computeRows :: Grid Int -> Matrix DL Int
computeRows = mapRows computeBidirectionalVisibleTrees

-- | Create the number of visible trees for all columns. This the product of the
-- computations from top to bottom and from bottom to top.
computeCols :: Grid Int -> Matrix DL Int
computeCols = mapCols computeBidirectionalVisibleTrees

-- | Compute the number of visible trees for every element in a row. This only
-- counts from left to right. Use 'computeBidirectionalVisibleTrees' to also
-- compute the right to left version at the same time.
computeVisibleTrees :: A.Manifest r Int => Vector r Int -> Vector DL Int
computeVisibleTrees vec = A.makeArray A.Seq (A.size vec) go
  where
    go :: Ix1 -> Int
    go (Ix1 n) = numVisibleTrees $ A.drop (Sz n) vec

-- Compute the product of the left-to-right and right-to-left visible tree count
-- from every element in this vector.
computeBidirectionalVisibleTrees :: (A.Manifest r Int) => Vector r Int -> Vector D Int
computeBidirectionalVisibleTrees vec =
  A.zipWith
    (*)
    (A.compute @U . computeVisibleTrees $ vec)
    -- Laziness is great until it isn't. We want to reverse the row, compute the
    -- visibility, and reverse the result.
    (A.reverse' (Dim 1) . A.compute @U . computeVisibleTrees . A.compute @U . A.reverse' (Dim 1) $ vec)

-- | Starting from the first tree in the vector, returns how many trees can be
-- seen to the right of it. A tree with the same or higher height blocks line of
-- sight, but the first such tree is still counted. A sequence @3511@ results in
-- a value of 1, only counting the @5@ tree, while @31234@ results in a value of
-- 4.
numVisibleTrees :: A.Source r Int => Vector r Int -> Int
numVisibleTrees vec = fst $ A.foldlS go (0, False) (A.tail' vec)
  where
    firstHeight = A.head' vec

    -- The integer here is the number of visible trees, and the boolean
    -- indicates whether or not the view has been blocked (in which case no
    -- other trees may count)
    go :: (Int, Bool) -> Int -> (Int, Bool)
    go (n, True) _ = (n, True)
    go (n, False) height = (n + 1, height >= firstHeight)
