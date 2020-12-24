module Day23.Parser (Day23.Parser.parse) where

import Text.Parsec

parse :: String -> String -> Either ParseError [Int]
parse = Text.Parsec.parse inputFile

inputFile :: Parsec String () [Int]
inputFile = many1 Day23.Parser.digit

digit :: Parsec String () Int
digit = do
  n <- Text.Parsec.digit
  return (read [n])
