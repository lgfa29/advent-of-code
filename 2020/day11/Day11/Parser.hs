module Day11.Parser (Day11.Parser.parse) where

import Text.Parsec

parse :: String -> String -> Either ParseError [String]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [String]
inputFile = endBy (many cell) (char '\n')

cell :: Parsec String () Char
cell = try (char 'L')
       <|> try (char '#')
       <|> char '.'
