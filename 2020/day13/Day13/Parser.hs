module Day13.Parser (Day13.Parser.parse, parseIds) where

import Text.Parsec

parse :: String -> String -> Either ParseError (Int, [Int])
parse = Text.Parsec.parse inputFile

parseIds :: String -> String -> Either ParseError [Int]
parseIds = Text.Parsec.parse idsOnly

-- Parsing
inputFile :: Parsec String () (Int, [Int])
inputFile = do
  ts <- number
  char '\n'
  ids <- sepBy busId (char ',')
  return (ts, ids)

idsOnly :: Parsec String () [Int]
idsOnly = do snd <$> inputFile

busId :: Parsec String () Int
busId = (-1 <$ try (char 'x')) <|> number

number :: Parsec String () Int
number = do
  n <- many1 digit
  return (read n)
