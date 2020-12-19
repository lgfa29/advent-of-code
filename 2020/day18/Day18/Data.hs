module Day18.Data
(
  Token (..)
, Expression(..)
) where

data Token = Number {value :: Int} | OpPlus | OpMult | LeftParen | RightParen deriving (Show, Eq)
type Expression = [Token]
