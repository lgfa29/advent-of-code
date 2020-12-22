module Day20.Parser (Day20.Parser.parse) where

import Day20.Data
import Text.Parsec

parse :: String -> String -> Either ParseError [Tile]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [Tile]
inputFile = many1 tile

tile :: Parsec String () Tile
tile = do
  id <- Day20.Parser.tileId
  Tile id <$> tileContent

tileId :: Parsec String () Int
tileId = do
  string "Tile "
  n <- many1 digit
  char ':'
  char '\n'
  return (read n)

tileContent :: Parsec String () [String]
tileContent = manyTill tileLine (char '\n')

tileLine :: Parsec String () String
tileLine = do
  line <- many tilePixel
  char '\n'
  return line

tilePixel :: Parsec String () Char
tilePixel = try (char '.') <|> char '#'
