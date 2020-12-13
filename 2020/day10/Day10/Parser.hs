module Day10.Parser (Day10.Parser.parse) where

import Text.Parsec

parse :: String -> String -> Either ParseError [Int]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [Int]
inputFile = endBy number (char '\n')

number :: Parsec String () Int
number = do
  n <- many digit
  return (read n)
