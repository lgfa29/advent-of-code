module Day7.Parser (Day7.Parser.parse) where

import Text.Parsec
import qualified Day7.Data as Data

parse :: String -> String -> Either ParseError [Data.Rule]
parse = Text.Parsec.parse inputFile

-- Separators
ruleSep = char '.' >> try (() <$ newline <|> eof)

-- Parsing
inputFile :: Parsec String () [Data.Rule]
inputFile = sepEndBy rule ruleSep

rule :: Parsec String () Data.Rule
rule = do
  b <- container
  stmts <- [] <$ string "no other bags" <|> sepBy statement (string ", ")
  return (Data.Rule b stmts)

container :: Parsec String () Data.Bag
container = do
  c <- bag
  space
  tryPluralize "bag"
  space
  string "contain"
  space
  return c

statement :: Parsec String () Data.Statement
statement = do
  num <- many1 digit
  space
  b <- bag
  space
  tryPluralize "bag"
  return (Data.Statement (read num) b)

bag :: Parsec String () Data.Bag
bag = do
  adj <- word
  space
  Data.Bag adj <$> word

word :: Parsec String () String
word = many1 (noneOf " ")

tryPluralize :: String -> Parsec String () String
tryPluralize s = try (string (s ++ "s")) <|> string s
