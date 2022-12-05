{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Text.Parsec hiding (State, parse, stateInput)
import qualified Text.Parsec as Parsec
import Text.Parsec.String

main :: IO ()
main = do
  (!state, !instructions) <- parse <$> readFile "inputs/day-5.txt"

  putStrLn "Part 1:"
  print $! map (head . snd) . M.toList $ run instructions state

-- * Part 1

type Crate = Char

-- | A map of crate stacks indexed by the column number.
type State = IntMap [Crate]

data Instruction
  = -- | An instruction to move a crate from the first column to the second one.
    -- Instructions to move multiple crates one after the other have already
    -- been unrolled.
    Move Int Int
  deriving (Show)

-- | Apply a list of instructions on the state
run :: [Instruction] -> State -> State
run [] s = s
run (Move x y : is) s = run is (move x y s)

-- | Move a single element from the first column to the second column. Diverges
-- if the source column is empty or the column doesn't exist in the satte.
move :: Int -> Int -> State -> State
move x y s =
  let crate = head (s M.! x)
      -- We can just pop 'crate' from the source column and push it to the
      -- target column
      s' = M.adjust tail x s
   in M.adjust (crate :) y s'

parse :: String -> (State, [Instruction])
parse (lines -> input) =
  -- The state input doesn't need the column number line, and we don't need the
  -- empty line for the instructions. This split should also be done in the
  -- parser itself but for this AoC puzzle this works fine.
  let !stateInput = unlines . init $ takeWhile (/= "") input
      !instructionsInput = unlines . tail $ dropWhile (/= "") input
   in ( fromRight' $ Parsec.parse pState "state input" stateInput
      , fromRight' $ Parsec.parse pInstructions "instructions input" instructionsInput
      )
  where
    fromRight' (Right a) = a
    fromRight' e = error $ "That's not quite...right ;))))) " <> show e

pState :: Parser State
pState = transposeState <$> sepEndBy pLine endOfLine <* eof
  where
    -- \| This contains all the crates from a single horizontal line, so
    -- basically a transposed version of the state map we want. 'Nothing's will
    -- be added when the line doesn't contain a crate at the given column.
    pLine :: Parser [Maybe Crate]
    pLine = many pSegment

    -- \| Parses either @[X]@ or @   @, both optionally followed by another
    -- whitespace character.
    pSegment :: Parser (Maybe Crate)
    pSegment =
      ( (Just <$> (char '[' *> anyChar <* char ']'))
          <|> (Nothing <$ string "   ")
      )
        <* optional (char ' ')

-- | Transform the input representation of the state into a map of stacks. The
-- input is a list of rows, and each row is a list of @Maybe Crate@s indicating
-- the value of the corresponding column. This is either @Nothing@ if the column
-- was empty or it contains the letter/crate from that column.
transposeState :: [[Maybe Crate]] -> State
transposeState = foldr insertStateRow M.empty
  where
    insertStateRow :: [Maybe Crate] -> State -> State
    insertStateRow row acc =
      -- We need the column numbers here
      let numbered = zip row [1 ..]
       in foldl' insertState acc numbered

    insertState :: State -> (Maybe Crate, Int) -> State
    insertState acc (Nothing, _) = acc
    insertState acc (Just crate, column) =
      -- Either insert a singleton list containing the crate, or const it to the
      -- existing crate stack
      let go Nothing = Just [crate]
          go (Just crates) = Just $ crate : crates
       in M.alter go column acc

pInstructions :: Parser [Instruction]
pInstructions = concat <$> sepEndBy pInstruction endOfLine <* eof

-- | This returns multiple instructions if @n@ is higher than 1 in @move n from
-- x to y@.
pInstruction :: Parser [Instruction]
pInstruction =
  (\n x y -> replicate (read n) $ Move (read x) (read y))
    <$> (string "move " *> many1 digit)
    <*> (string " from " *> many1 digit)
    <*> (string " to " *> many1 digit)
