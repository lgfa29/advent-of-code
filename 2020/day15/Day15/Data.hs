module Day15.Data (Game (..)) where

import qualified Data.Map as Map

data Game = Game { memory :: Map.Map Int [Int]
                 , spoken :: Int
                 , round :: Int
                 }
instance Show Game where
  show (Game _ s r) = "Round " ++ show r ++ ": " ++ show s
