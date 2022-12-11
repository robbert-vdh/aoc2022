{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (sortOn)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char
import Data.Void (Void)

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-11.txt"

  putStrLn "Part 1:"
  print input

-- * Part 1

type Item = Int

-- | Describes a monkey's behavior as per the puzzle's description as well as
-- the number of items it currently has and the number of items it has
-- inspected.
data Monkey = Monkey
  { items :: [Item]
  , itemsInspected :: Int
  , operation :: Operation
  , test :: Test
  }
  deriving (Show)

-- | For every item the monkey holds the item's value (worry level) is
-- manipulated according to this operation.
data Operation = OpAdd Int | OpMul Int | OpSquare deriving (Show)

-- | If the value is divisible by the first integer, then the item is thrown to
-- the monkey with the second integer's index. Otherwise the item is thrown to
-- the monkey with the last integer's index.
data Test = DivisibleBy Int Int Int deriving (Show)

-- ** Parsing

type Parser = Parsec Void String

parse :: String -> [Monkey]
parse = fromRight' . Parsec.parse go "input"
  where
    go :: Parser [Monkey]
    go = getMonkeys . sortOn fst <$> sepEndBy pMonkey eol

    -- Sanity check just to make sure the input is correct.
    getMonkeys :: [(Int, Monkey)] -> [Monkey]
    getMonkeys xs =
      if map fst xs == [0 .. length xs - 1]
        then map snd xs
        else error "Invalid monkey indices"

    fromRight' (Right a) = a
    fromRight' (Left e) = error $ "That's not quite...right ;))))) " <> errorBundlePretty e

-- This parses to the monkey's data, and its index. The input is in the correct
-- order but we'll double check just to be sure.
pMonkey :: Parser (Int, Monkey)
pMonkey =
  (\n s o t -> (n, Monkey s 0 o t))
    <$> pMonkeyIndex
    <*> pStartingItems
    <*> pOperation
    <*> pTest
  where
    pInt :: Parser Int
    pInt = read <$> some digitChar

    pMonkeyIndex :: Parser Int
    pMonkeyIndex = string "Monkey " *> pInt <* char ':' <* eol

    pStartingItems :: Parser [Int]
    pStartingItems = string "  Starting items: " *> sepBy pInt (string ", ") <* eol

    pOperation, pOpAdd, pOpMulSquare, pOpMul, pOpSquare :: Parser Operation
    pOperation = string "  Operation: new = old " *> (pOpAdd <|> pOpMulSquare) <* eol
    pOpAdd = OpAdd <$> (string "+ " *> pInt)
    -- Mul and square share a prefix, so if we parse it like this we don't need backtracking
    pOpMulSquare = string "* " *> (pOpMul <|> pOpSquare)
    pOpMul = OpMul <$> pInt
    pOpSquare = OpSquare <$ string "old"

    pTest :: Parser Test
    pTest =
      DivisibleBy
        <$> (string "  Test: divisible by " *> pInt <* eol)
        <*> (string "    If true: throw to monkey " *> pInt <* eol)
        <*> (string "    If false: throw to monkey " *> pInt <* eol)
