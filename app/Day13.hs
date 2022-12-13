{-# LANGUAGE BangPatterns #-}

module Main where

import Data.Void
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Parsec
import Text.Megaparsec.Char

main :: IO ()
main = do
  !input <- parse <$> readFile "inputs/day-13.txt"

  putStrLn "Part 1:"
  print $! score $ map (uncurry verifyOrder) input

-- * Part 1

data Packet = PacketInt Int | PacketList [Packet] deriving (Show)

-- | Determine the final part 1 score for a list of 'verifyOrder' results
score :: [Bool] -> Int
score = sum . zipWith (\idx result -> if result then idx else 0) [1..]

-- | Check whether the two packets are in the correct order according to the
-- bizarre rules from the puzzle.
verifyOrder :: Packet -> Packet -> Bool
verifyOrder l r = case verifyOrder' l r of
  Just result -> result
  Nothing -> error "Well uh yeah"

-- | Returns @Just True@ if the inputs are in the right order, @Just False@ if
-- they are not, and @Nothing@ if this was yet identified and the comparison
-- algorithm should continue with the next element.
verifyOrder' :: Packet -> Packet -> Maybe Bool
verifyOrder' (PacketInt l) (PacketInt r)
  | l < r = Just True
  | l > r = Just False
  | otherwise = Nothing
-- When two lists are identical parsing should continue as normal
verifyOrder' (PacketList []) (PacketList []) = Nothing
verifyOrder' (PacketList []) (PacketList _) = Just True
verifyOrder' (PacketList _) (PacketList []) = Just False
verifyOrder' (PacketList (l : ls)) (PacketList (r : rs)) =
  verifyOrder' l r <|> verifyOrder' (PacketList ls) (PacketList rs)
verifyOrder' ls@(PacketList _) (PacketInt r) =
  verifyOrder' ls (PacketList [PacketInt r])
verifyOrder' (PacketInt l) rs@(PacketList _) =
  verifyOrder' (PacketList [PacketInt l]) rs

-- ** Parsing

type Parser = Parsec Void String

parse :: String -> [(Packet, Packet)]
parse = fromRight' . Parsec.parse (sepEndBy pPacketPair eol <* eof) "input"
  where
    fromRight' (Right a) = a
    fromRight' (Left e) = error $ "That's not quite...right ;))))) " <> errorBundlePretty e

pPacketPair :: Parser (Packet, Packet)
pPacketPair = (,) <$> (pPacket <* eol) <*> (pPacket <* eol)

pPacket :: Parser Packet
pPacket = pPacketList <|> pPacketInt

pPacketList :: Parser Packet
pPacketList = PacketList <$> between (char '[') (char ']') (sepBy pPacket (char ','))

pPacketInt :: Parser Packet
pPacketInt = PacketInt . read <$> some digitChar
