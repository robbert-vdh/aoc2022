{-# LANGUAGE BangPatterns #-}

module Main where

main :: IO ()
main = do
  !input <- readFile "inputs/day-6.txt"

  putStrLn "Part 1:"
  print $! packetEndPos input

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
