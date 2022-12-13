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
  print input

-- * Part 1

data Packet = PacketInt Int | PacketList [Packet] deriving (Show)

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
