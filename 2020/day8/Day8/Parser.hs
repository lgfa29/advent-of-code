module Day8.Parser (Day8.Parser.parse) where

import qualified Day8.Data as Data
import Text.Parsec


parse :: String -> String -> Either ParseError [Data.Instruction]
parse = Text.Parsec.parse inputFile

inputFile :: Parsec String () [Data.Instruction]
inputFile = sepEndBy instruction newline

instruction :: Parsec String () Data.Instruction
instruction = do
  o <- operation
  space
  Data.Instruction o <$> argument

operation :: Parsec String () String
operation = try (string "acc")
            <|> try (string "jmp")
            <|> string "nop"
            <?> "operation"

argument :: Parsec String () Int
argument = signedInt

signedInt :: Parsec String () Int
signedInt = do
  sign <- try (char '-') <|> try (char '+') <|> return '+'
  num <- many1 digit
  let n = read num :: Int
  return (if sign == '-' then -1 * n else n)
