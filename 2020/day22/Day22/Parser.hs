module Day22.Parser (Day22.Parser.parse) where

import Day22.Data
import Text.Parsec

parse :: String -> String -> Either ParseError Game
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () Game
inputFile = do
  p1 <- player
  p2 <- player
  return (Game p1 p2)

player :: Parsec String () [Int]
player = do
  string "Player"
  space
  try (char '1') <|> char '2'
  char ':'
  newline
  manyTill (number <* char '\n') (() <$ char '\n' <|> eof)

number :: Parsec String () Int
number = do
  n <- many1 digit
  return (read n)
