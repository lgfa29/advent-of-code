module Day21.Parser (Day21.Parser.parse) where

import Day21.Data
import Text.Parsec

parse :: String -> String -> Either ParseError [Food]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [Food]
inputFile = sepEndBy1 food (char '\n')

food :: Parsec String () Food
food = do
  ings <- sepEndBy1 ingredient space
  algs <- try foodAllergens <|> return []
  return (Food ings algs)

ingredient :: Parsec String () String
ingredient = many1 letter

foodAllergens :: Parsec String () [String]
foodAllergens = do
  spaces
  char '('
  string "contains"
  spaces
  aller <- sepBy1 (many letter) (string ", ")
  char ')'
  return aller
