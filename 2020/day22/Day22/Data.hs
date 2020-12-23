module Day22.Data
(
  Game (..)
, Player (..)
) where

data Player = Player1 | Player2 deriving (Show, Eq, Enum)
data Game = Game { player1 :: [Int]
                 , player2 :: [Int]
                 } deriving (Show, Eq)
