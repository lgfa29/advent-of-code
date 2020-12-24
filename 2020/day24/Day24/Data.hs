module Day24.Data (
  Color (..)
, Direction (..)
, Floor (..)
)
where

import qualified Data.Map as Map

data Color = Black | White deriving (Show, Eq, Bounded, Enum)
data Direction = E | SE | SW | W | NW | NE deriving (Show, Eq, Enum)
type Floor = Map.Map (Int, Int) Color
