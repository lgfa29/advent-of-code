module Day24.Parser (Day24.Parser.parse) where

import Day24.Data
import Text.Parsec

parse :: String -> String -> Either ParseError [[Direction]]
parse = Text.Parsec.parse inputFile

-- Parsing
inputFile :: Parsec String () [[Direction]]
inputFile = sepEndBy instrucion (char '\n')

instrucion :: Parsec String () [Direction]
instrucion = many1 direction

direction :: Parsec String () Direction
direction = try directionE
            <|> try directionSE
            <|> try directionSW
            <|> try directionW
            <|> try directionNW
            <|> directionNE

directionE :: Parsec String () Direction
directionE = string "e" >> return E

directionSE :: Parsec String () Direction
directionSE = string "se" >> return SE

directionSW :: Parsec String () Direction
directionSW = string "sw" >> return SW

directionW :: Parsec String () Direction
directionW = string "w" >> return W

directionNW :: Parsec String () Direction
directionNW = string "nw" >> return NW

directionNE :: Parsec String () Direction
directionNE = string "ne" >> return NE
