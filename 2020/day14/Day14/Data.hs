module Day14.Data
(
  Computer (..)
, Instruction (..)
, Operation (..)
)
where

import qualified Data.Map as Map

data Operation = Mask | Mem deriving (Enum, Show, Eq)

data Instruction = Instruction { op :: Operation
                               , attr :: Int
                               , arg :: String
                               } deriving (Show)

data Computer = Computer { memory :: Map.Map Int Int
                         , mask :: String
                         } deriving (Show)
