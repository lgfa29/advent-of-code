module Day14.Parser (Day14.Parser.parse) where

import Day14.Data
import Text.Parsec

parse :: String -> String -> Either ParseError [Instruction]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [Instruction]
inputFile = endBy instruction (char '\n')

instruction :: Parsec String () Instruction
instruction = try Day14.Parser.mask <|> Day14.Parser.memory

mask :: Parsec String () Instruction
mask = do
  string "mask"
  opArgSep
  v <- many maskbit
  return (Instruction Mask 0 v)

memory :: Parsec String () Instruction
memory = do
  string "mem"
  char '['
  n <- many digit
  char ']'
  opArgSep
  v <- many digit
  return (Instruction Mem (read n) v)

maskbit :: Parsec String () Char
maskbit = try (char 'X') <|> bit

bit :: Parsec String () Char
bit = try (char '1') <|> char '0'

-- Separators
opArgSep = space >> char '=' >> space
