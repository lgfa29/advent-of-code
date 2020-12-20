module Day19.Parser (Day19.Parser.parse) where

import Text.Parsec
import qualified Data.Map as Map
import qualified Day19.Data as Data

parse :: String -> String -> Either ParseError (Data.Rules, [String])
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () (Data.Rules, [String])
inputFile = do
  rs <- Day19.Parser.rules
  msgs <- sepEndBy1 (many1 (noneOf "\n")) (char '\n')
  return (rs, msgs)

rules :: Parsec String () Data.Rules
rules = do
  rs <- manyTill rule (try (string "\n\n"))
  return (Map.fromList rs)

rule :: Parsec String () (Int, Data.Rule)
rule = do
  optional (char '\n')
  i <- number
  char ':'
  spaces
  r <- try subrule <|> singleChar
  return (i, r)

subrule :: Parsec String () Data.Rule
subrule = do
  subr <- sepBy1 listOfNumbers (string "| ")
  return (Data.SubRule subr)

singleChar :: Parsec String () Data.Rule
singleChar = do
  char '"'
  c <- anyChar
  char '"'
  return (Data.SingleChar c)

listOfNumbers :: Parsec String () [Int]
listOfNumbers = sepEndBy1 number (string " ")

number :: Parsec String () Int
number = do
  n <- many1 digit
  return (read n)
