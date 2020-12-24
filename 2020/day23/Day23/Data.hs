module Day23.Data (Game (..)) where

import qualified Data.IntMap as IntMap

data Game = Game { current :: Int
                 , cups :: IntMap.IntMap Int
                 } deriving (Show)
