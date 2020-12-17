module Day16.Parser (Day16.Parser.parse) where

import qualified Data.Map    as Map
import           Text.Parsec

import qualified Day16.Data  as Data

parse :: String -> String -> Either ParseError Data.Input
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () Data.Input
inputFile = do
  f <- fields
  mt <- myTicket
  nt <- nearbyTickets
  eof
  return (Data.Input f mt nt)

fields :: Parsec String () Data.Rules
fields = do
  f <- manyTill field (try (string "\n\n"))
  return (Map.fromList f)

field :: Parsec String () (String, Data.Rule)
field = do
  optional (char '\n')
  k <- many1 (noneOf ":")
  char ':'
  space
  v <- sepBy1 range rangeSep
  return (k, v)

range :: Parsec String () (Int, Int)
range = do
  a <- number
  char '-'
  b <- number
  return (a, b)

myTicket :: Parsec String () Data.Ticket
myTicket = do
  string "your ticket:"
  char '\n'
  n <- numberList
  char '\n'
  char '\n'
  return n

nearbyTickets :: Parsec String () [Data.Ticket]
nearbyTickets = do
  string "nearby tickets:"
  char '\n'
  sepEndBy1 numberList (char '\n')

number :: Parsec String () Int
number = do
  n <- many1 digit
  return (read n)

numberList :: Parsec String () [Int]
numberList = sepBy1 number (char ',')

-- Separators
rangeSep = string " or "
