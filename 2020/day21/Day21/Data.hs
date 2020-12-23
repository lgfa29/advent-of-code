module Day21.Data
(
  AllergenCount (..)
, Food (..)
) where

import qualified Data.Map as Map

type AllergenCount = Map.Map String Int

data Food = Food { ingredients :: [String]
                 , allergens :: [String]
                 } deriving (Show)
