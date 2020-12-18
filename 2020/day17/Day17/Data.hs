module Day17.Data (
  Coord (..)
, Coord4D (..)
, Cube (..)
, CubeSet (..)
, CubeSet4D (..)
) where

import qualified Data.Map as Map

data Coord = Coord { x :: Int
                   , y :: Int
                   , z :: Int
                   } deriving (Eq, Ord)
instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

data Coord4D = Coord4D { a :: Int
                       , b :: Int
                       , c :: Int
                       , d :: Int
                   } deriving (Eq, Ord)
instance Show Coord4D where
  show (Coord4D a b c d) = "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ ")"

newtype Cube = Cube { active :: Bool }
instance Show Cube where
  show (Cube a) = if a then "#" else "."

type CubeSet = Map.Map Coord Cube
type CubeSet4D = Map.Map Coord4D Cube
