module Day25.Parser (Day25.Parser.parse) where

import Text.Parsec

parse :: String -> String -> Either ParseError (Int, Int)
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () (Int, Int)
inputFile = do
  k1 <- number
  newline
  k2 <- number
  return (k1, k2)

number :: Parsec String () Int
number = do
  n <- many1 digit
  return (read n)
