module Day19.Data
(
  Rule (..)
, Rules (..)
) where

import qualified Data.Map as Map

data Rule = SubRule { rules :: [[Int]] } | SingleChar { char :: Char } deriving (Show)
type Rules = Map.Map Int Rule
