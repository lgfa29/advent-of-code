module Day12.Data
(
  Instruction (..)
, Action (..)
, Ship (..)
, ShipWithWaypoint (..)
, Waypoint (..)
) where

data Action = N | S | E | W | L | R | F deriving (Enum, Eq, Show)

data Instruction = Instruction { action :: Action, value :: Int } deriving (Eq)
instance Show Instruction where
  show (Instruction a v) = show a ++ show v

data Ship = Ship { east :: Int
                 , north :: Int
                 , direction :: Action
                 }
instance Show Ship where
  show (Ship e n d) = show d ++ " (" ++ show e ++ ", " ++ show n ++ ")"

data Waypoint = Waypoint { wpEast :: Int
                         , wpNorth :: Int
                         }
instance Show Waypoint where
  show (Waypoint e n) = "(" ++ show e ++ ", " ++ show n ++ ")"

data ShipWithWaypoint = ShipWithWaypoint { ship :: Ship
                                         , waypoint :: Waypoint
                                         }
instance Show ShipWithWaypoint where
  show (ShipWithWaypoint s w) = show s ++ " " ++ show w
