module Day20.Data
(
  Image (..)
, Tile (..)
) where

import qualified Data.List as List

newtype Image = Image { img :: [String] } deriving (Eq)
instance Show Image where
  show (Image i) = List.intercalate "\n" i

data Tile = Tile { tileId :: Int
                 , content :: [String]
                 } deriving (Eq)
instance Show Tile where
  show (Tile tId c) = "Tile " ++ show tId ++ ":\n" ++ List.intercalate "\n" c
