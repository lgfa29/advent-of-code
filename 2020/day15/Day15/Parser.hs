module Day15.Parser (Day15.Parser.parse) where

import Text.Parsec

parse :: String -> String -> Either ParseError [Int]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [Int]
inputFile = sepBy number (char ',')

number :: Parsec String () Int
number = do
  n <- many digit
  return (read n)
