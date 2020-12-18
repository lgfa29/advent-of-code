module Day17.Parser (Day17.Parser.parse) where

import Day17.Data
import Text.Parsec
import qualified Data.Map as Map

parse :: String -> String -> Either ParseError CubeSet
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () CubeSet
inputFile = do
  lines <- sepEndBy line (char '\n')
  let ys = zip [0..] lines
  let xs = foldl (\acc (y, line) -> acc ++ zip [Coord x y 0 | x <- [0..]] line) [] ys
  return (Map.fromList xs :: CubeSet)

line :: Parsec String () [Cube]
line = many1 cube

cube :: Parsec String () Cube
cube = do
  a <- True <$ try (char '#') <|> False <$ char '.'
  return (Cube a)
