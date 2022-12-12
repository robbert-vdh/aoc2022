{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec hiding (State, parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-11.txt"

  putStrLn "Part 1:"
  let !p1Result = execState (replicateM_ 20 (processRound True)) input
  print $! monkeyBusiness p1Result

  putStrLn "\nPart 2:"
  let !p2Result = execState (replicateM_ 10000 (processRound False)) input
  print $! monkeyBusiness p2Result

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
data Operation = OpAdd Item | OpMul Item | OpSquare deriving (Show)

-- | If the value is divisible by the first integer, then the item is thrown to
-- the monkey with the second integer's index. Otherwise the item is thrown to
-- the monkey with the last integer's index.
data Test = DivisibleBy Item Int Int deriving (Show)

type MonkeyState a = State (Vector Monkey) a

-- | Compute the product of the two highest 'itemsInspected' values.
monkeyBusiness :: Vector Monkey -> Int
monkeyBusiness = product . take 2 . sortOn Down . map itemsInspected . V.toList

-- | Process an entire round of the monkey business. All monkeys inspect and
-- throw items in sequence, starting with the monkey with index 0.
--
-- The division by three only happens if @shouldDivide@ is true.
processRound :: Bool -> MonkeyState ()
processRound shouldDivide = do
  n <- gets V.length
  mapM_ (processMonkey shouldDivide) [0 .. n - 1]

  -- Dividing each value by the product of all test values keeps the worry
  -- levels manageable
  unless shouldDivide doModLcm

-- | Process all operations for the monkey with the given index as described in
-- the puzzle. If the index is out of bounds for the monkeys vector this
-- diverges. For every item the monkey currently possesses, it first modifies
-- the item's worry level according to the operation, divides the new worry
-- level by three, passes the item to another monkey based on the test, and then
-- increases the number of items handled by 1.
processMonkey :: Bool -> Int -> MonkeyState ()
processMonkey shouldDivide idx = do
  monkey@Monkey {items, itemsInspected, operation, test} <- gets (V.! idx)
  let numItems = length items
      -- Every item's worry level changes and is then thrown to another monkey.
      -- This yields a list of `(newMonkeyIdx, newWorryLevel)` pairs
      thrownItems = map (throwItem shouldDivide operation test) items

  -- The monkey has inspected all of its items and passed them to other monkeys
  -- NOTE: This is an invariant on the input that should always be true, so
  --       we'll just assume it is
  modify' (V.// [(idx, monkey {items = [], itemsInspected = itemsInspected + numItems})])

  -- The thrown items now need to be added to their new rightful owners
  mapM_ (uncurry addItem) thrownItems

-- | Add an item to the monkey with the given index.
addItem :: Int -> Item -> MonkeyState ()
addItem newIdx newItem = do
  newMonkey@Monkey {items} <- gets (V.! newIdx)
  modify' (V.// [(newIdx, newMonkey {items = newItem : items})])

-- | Based on the operation and test, update an item's worry level and determine
-- which monkey it should be thrown at.
throwItem :: Bool -> Operation -> Test -> Item -> (Int, Item)
throwItem shouldDivide op (DivisibleBy n l r) worry =
  let worry' = updateWorry shouldDivide op worry
   in if (worry' `rem` n) == 0
        then (l, worry')
        else (r, worry')

-- | Update an item's worry level by applying the operation and then dividing
-- the result by 3 (because the monkey didn't destroy the item).
updateWorry :: Bool -> Operation -> Item -> Item
updateWorry shouldDivide op worry =
  let worry' = case op of
        OpAdd x -> worry + x
        OpMul x -> worry * x
        OpSquare -> worry * worry
   in if shouldDivide
        then worry' `div` 3
        else worry'

-- ** Parsing

type Parser = Parsec Void String

parse :: String -> Vector Monkey
parse = V.fromList . fromRight' . Parsec.parse go "input"
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

    pItem :: Parser Item
    pItem = read <$> some digitChar

    pMonkeyIndex :: Parser Int
    pMonkeyIndex = string "Monkey " *> pInt <* char ':' <* eol

    pStartingItems :: Parser [Item]
    pStartingItems = string "  Starting items: " *> sepBy pItem (string ", ") <* eol

    pOperation, pOpAdd, pOpMulSquare, pOpMul, pOpSquare :: Parser Operation
    pOperation = string "  Operation: new = old " *> (pOpAdd <|> pOpMulSquare) <* eol
    pOpAdd = OpAdd <$> (string "+ " *> pItem)
    -- Mul and square share a prefix, so if we parse it like this we don't need backtracking
    pOpMulSquare = string "* " *> (pOpMul <|> pOpSquare)
    pOpMul = OpMul <$> pItem
    pOpSquare = OpSquare <$ string "old"

    pTest :: Parser Test
    pTest =
      DivisibleBy
        <$> (string "  Test: divisible by " *> pItem <* eol)
        <*> (string "    If true: throw to monkey " *> pInt <* eol)
        <*> (string "    If false: throw to monkey " *> pInt <* eol)

-- * Part 2

-- | Compute the least common multiple of all worry levels and replace all worry
-- levels by `x % lcm` to avoid the values from growing too large.
doModLcm :: MonkeyState ()
doModLcm = do
  lcm' <- product <$> gets (V.map (testValue . test))
  modify' $ V.map (\monkey -> monkey {items = map (`rem` lcm') (items monkey)})

  where
    testValue (DivisibleBy n _ _) = n
