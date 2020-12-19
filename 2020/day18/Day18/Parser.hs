module Day18.Parser (Day18.Parser.parse) where

import Text.Parsec
import Day18.Data

parse :: String -> String -> Either ParseError [Expression]
parse = Text.Parsec.parse inputFile

inputFile :: Parsec String () [Expression]
inputFile = sepEndBy expression (char '\n')

expression :: Parsec String () Expression
expression = do
  t <- Day18.Parser.token
  optional (char ' ')
  ex <- optionMaybe expression
  case ex of
    Nothing -> return [t]
    Just ex -> return (t:ex)

token :: Parsec String () Token
token = try leftParen
        <|> try rightParen
        <|> try number
        <|> operator

leftParen :: Parsec String () Token
leftParen = do
  char '('
  return LeftParen

rightParen :: Parsec String () Token
rightParen = do
  char ')'
  return RightParen

number :: Parsec String () Token
number = do
  n <- many1 digit
  return (Number (read n))

operator :: Parsec String () Token
operator = try operatorPlus <|> operatorMult

operatorPlus :: Parsec String () Token
operatorPlus = do
  char '+'
  return OpPlus

operatorMult :: Parsec String () Token
operatorMult = do
  char '*'
  return OpMult
