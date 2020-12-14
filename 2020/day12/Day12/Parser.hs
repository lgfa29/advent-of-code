module Day12.Parser (Day12.Parser.parse) where

import qualified Day12.Data as Data
import Text.Parsec

parse :: String -> String -> Either ParseError [Data.Instruction]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [Data.Instruction]
inputFile = endBy instruction (char '\n')

instruction :: Parsec String () Data.Instruction
instruction = do
  a <- action
  Data.Instruction a <$> value

action :: Parsec String () Data.Action
action = Data.N <$ try (char 'N')
     <|> Data.S <$ try (char 'S')
     <|> Data.E <$ try (char 'E')
     <|> Data.W <$ try (char 'W')
     <|> Data.L <$ try (char 'L')
     <|> Data.R <$ try (char 'R')
     <|> Data.F <$ char 'F'
     <?> "action"

value :: Parsec String () Int
value = do
  v <- many digit
  return (read v)
